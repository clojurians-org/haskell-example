{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Page.DataSandbox.DataService.FileService.SFTP
  (dataService_sftp) where

import Common.Types
import Common.WebSocketMessage
import Frontend.FrontendStateT
import Frontend.Class
import Prelude

import GHC.Int (Int64)
import qualified Data.HashMap.Lazy as M
import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (mapMaybe)

import Control.Concurrent (MVar, readMVar)
import Data.String.Conversions (cs)

import Frontend.Widget
import Control.Lens hiding (lens)
import Labels

import Data.Maybe (mapMaybe)

dataService_sftp
  :: forall t m.
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , HasFrontendState t (FaaSCenter, WSResponseMessage) m
     , EventWriter t [WSRequestMessage] m)
  => m ()
dataService_sftp = do
  st :: Dynamic t (FaaSCenter, WSResponseMessage) <- askFrontendState
  let sftpsD = st <&> (^.. _1 . lens #dataSandbox . lens #dataServices . each. _DSE_FileService_SFTP)

  dynText (cs . show <$> sftpsD)
  divClass "ui segment basic" $ do
    pageHeader "SFTP文件服务" ["实时写入", "支持CSV与JSON格式"]

  divClass "ui segment basic" $ do
    -- tableInfo sftpsD
    sftpE <- toTable sftpsD

    sftpD <- holdDyn def sftpE
    submitE <- loginFormEB (dsefsSFtpHost <$> sftpD) (dsefsSFtpUsername <$> sftpD) (dsefsSFtpPassword <$> sftpD)

    tellEvent (submitE <&> (:[]). flip DSEFSSFtpFileRREQ Nothing)

--    submitD <- holdDyn (credential "host" "username" "password") submitE
--    display submitD
--    tellEvent 

  divClass "ui segment basic" $ do
    divClass "ui top attached segment" $ do
--      elClass "h4" "ui header" $ text "文件浏览器"
      divClass "ui breadcrumb" $ do
        elClass "a" "section" $ text "."
        elClass "i"  "right chevron icon divider" $ blank
        elClass "a" "section" $ text "xxx"
        elClass "i" "right arrow icon divider" $ blank
        divClass "active section" $ text "aaa.txt"
    divClass "ui attached segment" $
      divClass "ui form field" $ do
        blank
    
  return ()
