{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.Page.DataNetwork.OneClickRun (dataNetwork_oneClickRun) where

import Common.WebSocketMessage
import Prelude

import Reflex.Dom.Core
import Control.Monad (forM_, void)
import Control.Monad.Fix (MonadFix)

import qualified Data.Text as T
import Data.String.Conversions (cs)

exampleCode :: String
exampleCode = unlines [
          "do"
        , "  let sql = [str|select"
        , "                |  id, name, description, 'type'"
        , "                |, state, timeliness, params, result_plugin_type"
        , "                |, vendor_id, server_id, success_code"
        , "                |from tb_interface"
        , "                |] :: B.ByteString"
        , "  let pgSettings = H.settings \"10.132.37.200\" 5432 \"monitor\" \"monitor\" \"monitor\""
        , "  let (curName, cursorSize, chanSize) = (\"larluo\", 200, 1000)"
        , "  let textColumn = HD.column HD.text"
        , "  let mkRow = (,,,,,,,,,,)"
        , "                <$> fmap (#id :=) textColumn"
        , "                <*> fmap (#name :=) textColumn"
        , "                <*> fmap (#description :=) textColumn"
        , "                <*> fmap (#type :=) textColumn"
        , "                <*> fmap (#state :=) textColumn"
        , "                <*> fmap (#timeliness :=) textColumn"
        , "                <*> fmap (#params :=) textColumn"
        , "                <*> fmap (#result_plugin_type :=) textColumn"
        , "                <*> fmap (#vendor_id :=) textColumn"
        , "                <*> fmap (#server_id :=) textColumn"
        , "                <*> fmap (#success_code :=) textColumn"
        , "  Right connection <- liftIO $ H.acquire pgSettings"
        , "  runResourceT $ do"
        , "    chan <- pgToChan connection sql curName cursorSize chanSize mkRow"
        , "    runConduit $"
        , "      (sourceTBMChan chan"
        , "            .| C.concat"
        , "            .| C.take 2"
        , "            .| C.mapM_ (liftIO . print))"
        ]

dataNetwork_oneClickRun
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t (Maybe WSResponseMessage)
  -> Event t WSResponseMessage
  -> m (Event t WSRequestMessage)
dataNetwork_oneClickRun wsInit wsEvt = do
  let wsHaskellCodeRunEvt = fforMaybe wsEvt $ \case
        (WSResponseMore (HaskellCodeRunResponse r)) -> Just r
        _ -> Nothing
  divClass "ui segment basic" $
    divClass "ui form" $ do
      myInput <- divClass "ui field" $ do
        textAreaElement $ def & initialAttributes .~ ("rows" =: "20")
                              & textAreaElementConfig_initialValue .~ (cs exampleCode)

      runEvt :: (Event t ()) <- divClass "ui field" $ do
        domEvent Click . fst <$> elClass' "button" "ui button teal" (text "RUN")

      divClass "ui field" $
        textAreaElement $ def & initialAttributes .~ ("rows" =: "10")
                              & textAreaElementConfig_initialValue .~ ""
                              & textAreaElementConfig_setValue .~ (fmap (cs . show) wsHaskellCodeRunEvt)
      return $ tag (fmap HaskellCodeRunRequest . current . value $ myInput) runEvt

