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
import Frontend.Widget
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


import Control.Lens hiding (lens)
import Labels

import Control.Applicative (liftA2)
import Data.Maybe (mapMaybe)
import System.FilePath (takeDirectory, takeFileName)

dataService_sftp
  :: forall t m.
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , HasFrontendState t (FaaSCenter, WSResponseMessage) m
     , EventWriter t [WSRequestMessage] m)
  => m ()
dataService_sftp = do
  (stD, msgD) :: (Dynamic t FaaSCenter, Dynamic t WSResponseMessage) <- splitDynPure <$> askFrontendState
  let  sftpsD = stD <&> (^.. lens #dataSandbox . lens #dataServices . each. _DSE_FileService_SFTP)
       directoryRE = fforMaybe (updated msgD) $ \case
        DSEFSSFtpDirectoryRRES (Right r) ->  Just r
        _ -> Nothing
  directoryRD <- holdDyn [] directoryRE
--  dynText (cs . show <$> msgD)
  divClass "ui segment basic" $ do
    pageHeader "SFTP文件服务" ["实时写入", "支持CSV与JSON格式"]

  (sftpD, submitD, submitE) <- divClass "ui segment basic" $ do
    sftpE <- toTable sftpsD

    sftpD <- holdDyn def sftpE
    (submitD, submitE) <- loginFormB (dsefsSFtpHost <$> sftpD) (dsefsSFtpUsername <$> sftpD) (dsefsSFtpPassword <$> sftpD)

    tellEventSingle (tagPromptlyDyn (DSEFSSFtpDirectoryRREQ <$> submitD <*> (Just . dsefsSFtpFilePath <$> sftpD)) submitE)
    return (sftpD, submitD, submitE)


  submitClickedD <- holdDyn Nothing (Just () <$ submitE)
  dyn (maybe blank (const (selectorWidget submitD sftpD directoryRD) ) <$> submitClickedD)

  return ()
  where
    selectorWidget submitD sftpD directoryRD =
        divClass "ui segment basic" $ do
          divClass "ui top attached segment" $ do
            divClass "ui horizontal divided list" $ do
              divClass "item" $ divClass "ui small basic icon buttons" $ do
                buttonClass "ui icon button" $ elClass "i" "home icon" blank
                buttonClass "ui icon button" $ elClass "i" "umbrella icon" blank
              divClass "item" $ do
                divClass "ui right pointing basic label" $ text "文件路径"
                divClass "ui breadcrumb" $ do
        
--                  elClass "i"  "right chevron icon divider" $ blank
                  (filePathD, filePathE) <- divClass "ui input" $ dynInputB (sftpD <&> dsefsSFtpFilePath)
                  tellEventSingle (tagPromptlyDyn (DSEFSSFtpDirectoryRREQ <$> submitD <*> (Just <$> filePathD)) filePathE)
                  {--
                  elClass "a" "section" $ text "."
                  elClass "i"  "divider" $ text "/"
                  elClass "a" "section" $ text "xxx"
                  --}
                  elClass "i" "right arrow icon divider" $ blank
                  divClass "ui input" $ dynInputDB (sftpD <&> dsefsSFtpFilePattern)
--                  divClass "active section" $ text "aaa.txt"
          divClass "ui attached segment ui table" $
            el "tbody" $ do
              simpleList (directoryRD <&> sortOn (liftA2 (,) sftpEntryType sftpEntryName)) $ \f -> el "tr" $ do
                el "td" $ do
                  flip (elDynClass "i") blank $ f <&> (\case
                      SFtpFille -> "file icon outline"
                      SFtpDirectory -> "folder icon"
                      SFtpUnknown -> "") . sftpEntryType
                  dynText (sftpEntryName <$> f)
                el "td" $ dynText (formatByteSize . fromIntegral . sftpEntrySize <$> f)
                el "td" $ dynText (iso8601TimeFormat . sftpEntryCTime <$> f)
          return ()
  
