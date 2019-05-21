{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Obelisk.Backend

import Data.Dependent.Sum
import Data.Functor.Identity (Identity(..))

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- serve $ const $ return ()
      serve $ \case
        BackendRoute_Missing :=> Identity () -> return ()
  , _backend_routeEncoder = backendRouteEncoder
  }
