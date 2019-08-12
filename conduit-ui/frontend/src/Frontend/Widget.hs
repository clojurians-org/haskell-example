{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Widget where

import Common.WebSocketMessage
import Prelude
import Reflex.Dom.Core
import qualified Data.Text as T

import Control.Monad (void, forM)
import Control.Monad.Fix (MonadFix)
import Data.Functor ((<&>))
import Data.Semigroup (stimes)

buttonClass :: forall t m. (DomBuilder t m, PostBuild t m)
  => T.Text -> m () -> m (Event t ())
buttonClass cls s = elClass' "button" cls s <&> void . domEvent Click . fst

submitEB :: forall t m. (DomBuilder t m, PostBuild t m)
  => T.Text -> m (Event t ())
submitEB = buttonClass "ui button teal" . text
            
dynInputDB :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> m (Dynamic t T.Text)
dynInputDB sD = do
  pb <- getPostBuild
  (inputElement $ def & inputElementConfig_setValue .~ leftmost [ updated sD, tag (current sD) pb ])
    <&> value
  
dynInputFieldDB :: forall t m. (DomBuilder t m, PostBuild t m)
  => T.Text -> (Dynamic t T.Text) -> m (Dynamic t T.Text)
dynInputFieldDB s vD = divClass "field" $ el "label" $ text s >> dynInputDB vD
  
pageHeader :: forall t m. (DomBuilder t m)
  => T.Text -> [T.Text] -> m ()
pageHeader title xs = do
  divClass "ui grid" $ divClass "eight wide column" $ divClass "ui message" $ do
      elClass "h2" "ui header" $ text title
      elClass "ul" "list" $ do
        forM xs (el "li" . elClass "h4" "ui header" . text)
  return ()

theadList :: forall t m .(DomBuilder t m, PostBuild t m)
  => [T.Text] -> m ()
theadList xs = do
  el "thead" $ el "tr" $ do
    elClass "th" "" $ checkbox False def
    forM xs (elClass "th" "" . text)
  return ()

loginFormEB :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> Dynamic t T.Text -> Dynamic t T.Text -> m (Event t Credential)
loginFormEB hostD usernameD passwordD = do
  divClass "ui form compact" $ do
    divClass "fields" $ do
      hostD' <- dynInputFieldDB "主机" hostD
      usernameD' <- dynInputFieldDB "用户名" usernameD
      passwordD' <- dynInputFieldDB "密码" passwordD
      divClass "field" $ do
        el "label" $ elClass "i" "angle double down icon" blank
        submitEB "连接" <&> tagPromptlyDyn (credential <$> hostD' <*> usernameD' <*> passwordD') 
  
{--
tbodyList :: forall t m .
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t [[T.Text]] -> m ()
tbodyList xssD = do
  let len = xssD <&> length . head
  el "tbody" $ do
    elClass "tr" "warning" $ do
        el "td" $ elClass "i" "notched circle loading icon" blank
        stimes  len $
          el "td" $ divClass "ui mini input" $ inputElement def
    simpleList xssD $ \xsD ->
      el "tr" $ do
        elClass "td" "" $ checkbox False def
        el "td" $ inputElement def
    return ()
--}   
--    simpleList xss 
  
