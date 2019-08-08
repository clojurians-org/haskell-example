{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Page.DataSandbox.DataService.FileService.SFTP
  (dataService_sftp_handle, dataService_sftp) where

import Common.Types
import Common.WebSocketMessage
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

dataService_sftp_handle
    :: forall t m r v.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m
     , Has "dataSandbox" v r
     , Has "dataServices" (M.HashMap Int64 DataService) v
     )
  => Dynamic t r -> Event t WSResponseMessage
  -> m (Dynamic t [DSEFSSFtp], Event t WSResponseMessage)
dataService_sftp_handle stD wsResponseEvt = do
  let wsEvt = ffilter (const True) wsResponseEvt
      ftps st = view (lens #dataSandbox . lens #dataServices) st
                  & M.toList & mapMaybe (\case
                                          (k, DSE_FileService_SFTP ftp) -> Just ftp
                                          _ -> Nothing)
                  
  return (stD <&> ftps, wsEvt)

dataService_sftp
  :: forall t m.
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => (Dynamic t [DSEFSSFtp], Event t WSResponseMessage)
  -> m (Event t [WSRequestMessage])
dataService_sftp (sftpsD, wsEvt) = do
  dynText (cs . show <$> sftpsD)
  divClass "ui segment basic" $ do
    pageHeader "SFTP文件服务" ["实时写入", "支持CSV与JSON格式"]

  divClass "ui segment basic" $ do
    -- tableInfo sftpsD
    sftpE <- toTable sftpsD

    sftpD <- holdDyn def sftpE
    submitE <- loginFormEB (dsefsSFtpHost <$> sftpD) (dsefsSFtpUsername <$> sftpD) (dsefsSFtpPassword <$> sftpD)

    submitD <- holdDyn ("host", "username", "password") submitE
    dynText (cs . show <$> submitD)


  divClass "ui segment basic" $ do
      divClass "ui top attached segment" $ do
        elClass "h4" "ui header" $ text "文件浏览器"
      divClass "ui attached segment" $
        divClass "ui form field" $ do
          blank
    
  return never
