{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, PatternSynonyms, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeFamilies, UndecidableInstances #-}


module Frontend.FrontendStateT where

import Common.Api
import Common.Types
import Common.WebSocketMessage

import Prelude

import GHC.Int (Int64)
import Data.Coerce (coerce)
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as J
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Data.Functor ((<&>))
import Control.Monad.Fix (MonadFix)
import Data.Default (def)
import Data.Semigroup (Endo(..))
import Data.String.Conversions (cs)

import Reflex
import Obelisk.Route.Frontend
--  ( RoutedT, RouteToUrl(..), SetRoute(..)
--  , mapRoutedT, routeLink, askRoute, subRoute, subRoute_)

import Text.Regex.TDFA ((=~))
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import System.Random (randomRIO)
import Control.Concurrent
  (MVar, newMVar, swapMVar, threadDelay)

import Labels ((:=)(..), Has)

import Control.Monad.Trans (MonadTrans(lift), MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Reflex.Host.Class (MonadReflexCreateTrigger)

newtype FrontendStateT (t :: * ) s m a = FrontendStateT
  { unFrontendStateT :: ReaderT (Dynamic t s) m a }
  deriving ( Functor, Applicative, Monad, MonadFix, MonadTrans, MonadIO
           , MonadSample t, MonadHold t, PostBuild t, TriggerEvent t
           , PerformEvent t, DomBuilder t, NotReady t)

instance Adjustable t m => Adjustable t (FrontendStateT t s m) where
  runWithReplace a0 a' = FrontendStateT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = FrontendStateT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = FrontendStateT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = FrontendStateT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, RouteToUrl r m) => RouteToUrl r (FrontendStateT t s m) where
  askRouteToUrl = lift askRouteToUrl
instance (Monad m, SetRoute t r m) => SetRoute t r (FrontendStateT t s m) where
  setRoute = lift . setRoute
  modifyRoute = lift . modifyRoute

runFrontendStateT :: FrontendStateT t s m a -> Dynamic t s -> m a
runFrontendStateT t = runReaderT (unFrontendStateT t)

class HasFrontendState t s m | m -> s where
  askFrontendState :: m (Dynamic t s)

instance Monad m => HasFrontendState t s (FrontendStateT t s m) where
  askFrontendState = FrontendStateT ask

instance (Monad m, HasFrontendState t s m)  => HasFrontendState t s (RoutedT t r m) where
  askFrontendState = lift askFrontendState

instance (Monad m, HasFrontendState t s m)  => HasFrontendState t s (EventWriterT t w m) where
  askFrontendState = lift askFrontendState  