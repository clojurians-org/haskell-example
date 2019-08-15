{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Widget where

import Common.Types
import Common.WebSocketMessage
import Prelude
import Reflex.Dom.Core
import qualified Data.Text as T

import Data.Proxy (Proxy(..))
import Control.Monad (void, forM, forM_)
import Control.Monad.Fix (MonadFix)
import Data.Functor ((<&>))
import Data.Semigroup (stimes)
import qualified Data.Map as M
import Control.Lens hiding (element)


tellEventSingle :: forall t m a. (EventWriter t [a] m, Reflex t)
  => Event t a -> m ()
tellEventSingle x = tellEvent (fmap (:[]) x)

buttonClass :: forall t m. (DomBuilder t m, PostBuild t m)
  => T.Text -> m () -> m (Event t ())
buttonClass cls s = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
                & initialAttributes .~ ("class" =: cls)
                & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> stopPropagation)
                  
  element "button" cfg s <&> void . domEvent Click . fst

submitEB :: forall t m. (DomBuilder t m, PostBuild t m)
  => T.Text -> m (Event t ())
submitEB = buttonClass "ui button teal" . text

dynCheckboxDB :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t Bool -> m (Dynamic t Bool)
dynCheckboxDB bD= do
  pbE <- getPostBuild
  let config = def & setValue .~ leftmost [(updated bD), (current bD) <@ pbE]
  checkbox False config <&> value
  
dynInputB :: forall t m. (DomBuilder t m, PostBuild t m)
  =>  Dynamic t T.Text -> m (Dynamic t T.Text, Event t T.Text)
dynInputB sD = do
  pb <- getPostBuild
  inputDOM <- inputElement $ def & inputElementConfig_setValue .~ leftmost [ updated sD, tag (current sD) pb ]
  return (value inputDOM, tagPromptlyDyn (value inputDOM) (keypress Enter inputDOM))
  
dynInputDB :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> m (Dynamic t T.Text)
dynInputDB = fmap fst . dynInputB 

dynInputEB :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> m (Event t T.Text)
dynInputEB = fmap snd . dynInputB

dynEditorB :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> m ()
dynEditorB sD = do
  pbE <- getPostBuild
  textAreaElement $ def
      & initialAttributes .~ ("rows" =: "20")
      & textAreaElementConfig_initialValue .~ ""
      & textAreaElementConfig_setValue .~ leftmost [updated sD, (current sD) <@ pbE]
  return ()
  
dynEditor :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> m ()
dynEditor = dynEditorB
  

{--
dynInput :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> m ()
dynInput = void . dynInputB
--}
  
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

trHeadList :: forall t m .(DomBuilder t m, PostBuild t m)
  => [T.Text] -> m ()
trHeadList xs= forM_ xs (elClass "th" "" . text)

  
theadList :: forall t m .(DomBuilder t m, PostBuild t m)
  => [T.Text] -> m ()
theadList xs = do
  el "thead" $ el "tr" $ do
    elClass "th" "" $ checkbox False def
    trHeadList xs
  return ()

loginFormB :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> Dynamic t T.Text -> Dynamic t T.Text -> m (Dynamic t Credential, Event t Credential)
loginFormB hostD usernameD passwordD = do
  divClass "ui form compact" $ do
    divClass "fields" $ do
      hostD' <- dynInputFieldDB "主机" hostD
      usernameD' <- dynInputFieldDB "用户名" usernameD
      passwordD' <- dynInputFieldDB "密码" passwordD
      divClass "field" $ do
        el "label" $ elClass "i" "angle double down icon" blank
        let vD = credential <$> hostD' <*> usernameD' <*> passwordD'
        submitE <- submitEB "连接"
        return (vD, tagPromptlyDyn vD submitE)
  

loginFormDB :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> Dynamic t T.Text -> Dynamic t T.Text -> m (Dynamic t Credential)
loginFormDB = ((.) . (.) . (.)) (fmap fst) loginFormB
  
loginFormEB :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> Dynamic t T.Text -> Dynamic t T.Text -> m (Event t Credential)
loginFormEB = ((.) . (.) . (.)) (fmap snd)  loginFormB
  
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
  
