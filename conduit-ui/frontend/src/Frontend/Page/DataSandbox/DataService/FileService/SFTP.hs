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
  (stD, msgD) :: (Dynamic t FaaSCenter, Dynamic t WSResponseMessage) <- splitDynPure <$> askFrontendState
  let  sftpsD = stD <&> (^.. lens #dataSandbox . lens #dataServices . each. _DSE_FileService_SFTP)
       fileRE = fforMaybe (updated msgD) $ \case
        DSEFSSFtpFileRRES (Right r) ->  Just r
        _ -> Nothing
  fileRD <- holdDyn [] fileRE
--  dynText (cs . show <$> msgD)
  divClass "ui segment basic" $ do
    pageHeader "SFTP文件服务" ["实时写入", "支持CSV与JSON格式"]

  divClass "ui segment basic" $ do
    -- tableInfo sftpsD
    sftpE <- toTable sftpsD

    sftpD <- holdDyn def sftpE
    submitE <- loginFormEB (dsefsSFtpHost <$> sftpD) (dsefsSFtpUsername <$> sftpD) (dsefsSFtpPassword <$> sftpD)

    tellEvent (submitE <&> (:[]). flip DSEFSSFtpFileRREQ Nothing)
    
  divClass "ui segment basic" $ do
    divClass "ui top attached segment" $ do
--      elClass "h4" "ui header" $ text "文件浏览器"
      divClass "ui breadcrumb" $ do
        elClass "a" "section" $ text "."
--        elClass "i"  "right chevron icon divider" $ blank
        elClass "i"  "divider" $ text "/"
        elClass "a" "section" $ text "xxx"
        elClass "i" "right arrow icon divider" $ blank
        divClass "active section" $ text "aaa.txt"
    divClass "ui attached segment ui table" $
      el "tbody" $ do
        simpleList fileRD $ \f -> el "tr" $ do
          el "td" $ dynText (sftpEntryName <$> f)
          el "td" $ dynText (cs . show . sftpEntrySize <$> f)
          el "td" $ dynText (cs . show . sftpEntryCTime <$> f)
    
  return ()
