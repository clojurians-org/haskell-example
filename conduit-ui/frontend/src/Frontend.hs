{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend where

import Prelude
import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Control.Monad.IO.Class (MonadIO)

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Data.Default (def)

import Reflex.Dom.SemanticUI (
    form, field
  )

htmlHeader :: DomBuilder t m => m ()
htmlHeader = do
  elAttr "link" ( "rel" =: "stylesheet"
               <> "href" =: "https://cdn.jsdelivr.net/npm/semantic-ui@2.3.3/dist/semantic.min.css") blank

page :: DomBuilder t m => m ()
page = do
  divClass "ui segment basic" $ 
    divClass "ui form" $ do
      divClass "ui field" $
        textAreaElement $ def & textAreaElementConfig_initialValue .~ ""
                              & initialAttributes .~ ("rows" =: "10")
      divClass "ui field" $
        divClass "ui button blue" $ text "RUN"
      divClass "ui field" $
        textAreaElement $ def & textAreaElementConfig_initialValue .~ ""
                              & initialAttributes .~ ("rows" =: "10")
  return ()
  
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = htmlHeader
  , _frontend_body = do
      page
  }
