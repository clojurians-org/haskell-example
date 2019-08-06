{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Widget where

import Prelude
import Reflex.Dom.Core
import qualified Data.Text as T

import Control.Monad (void)

theadList
  :: forall t m .
     (DomBuilder t m, PostBuild t m)
  => [T.Text] -> m ()
theadList xs = do
  el "thead" $ el "tr" $ do
    elClass "th" "" $ checkbox False def
    void $ sequence $ map (elClass "th" "" . text) xs


tbodyList :: forall t m .
  (DomBuilder t m, PostBuild t m)
  =>Dynamic t [[T.Text]] -> m ()
tbodyList xss = do
  el "tbody" $ do
    elClass "tr" "warning" $ do
      el "td" $ elClass "i" "notched circle loading icon" blank
--        False -> elClass "th" "" $ checkbox False def
      el "td" $ divClass "ui mini input" $ inputElement def
      el "td" $ divClass "ui mini input" $ inputElement def
      el "td" $ divClass "ui mini input" $ inputElement def
    undefined
--    simpleList xss 
  
