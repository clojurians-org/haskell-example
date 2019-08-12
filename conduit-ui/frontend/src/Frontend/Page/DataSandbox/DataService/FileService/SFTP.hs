{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Page.DataSandbox.DataService.FileService.SFTP
  (dataService_sftp) where

import Common.Api
import Common.Types
import Common.WebSocketMessage
import Frontend.FrontendStateT
import Frontend.Class
import Prelude
import Data.List (sortOn)

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

import Control.Applicative (liftA2)
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
      divClass "ui horizontal divided list" $ do
        divClass "item" $ divClass "ui small basic icon buttons" $ do
          buttonClass "ui icon button" $ elClass "i" "home icon" blank
          buttonClass "ui icon button" $ elClass "i" "umbrella icon" blank
        divClass "item" $ do
          divClass "ui right pointing basic label" $ text "文件路径"
--        divClass "item" $
          divClass "ui breadcrumb" $ do

--            elClass "i"  "right chevron icon divider" $ blank
            divClass "ui input" $ inputElement def
            {--
            elClass "a" "section" $ text "."
            elClass "i"  "divider" $ text "/"
            elClass "a" "section" $ text "xxx"
            --}
            elClass "i" "right arrow icon divider" $ blank
            divClass "ui input" $ dynInputDB (constDyn "aaa.txt")
--            divClass "active section" $ text "aaa.txt"
    divClass "ui attached segment ui table" $
      el "tbody" $ do
        simpleList (fileRD <&> sortOn (liftA2 (,) sftpEntryType sftpEntryName)) $ \f -> el "tr" $ do
          el "td" $ do
            flip (elDynClass "i") blank $ f <&> (\case
                SFtpFille -> "file icon"
                SFtpDirectory -> "folder icon"
                SFtpUnknown -> "") . sftpEntryType
            dynText (sftpEntryName <$> f)
          el "td" $ dynText (formatByteSize . fromIntegral . sftpEntrySize <$> f)
          el "td" $ dynText (iso8601TimeFormat . sftpEntryCTime <$> f)
    
  return ()
