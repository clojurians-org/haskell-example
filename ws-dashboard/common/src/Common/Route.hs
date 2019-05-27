{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity
import Data.Functor.Sum

import Obelisk.Route
import Obelisk.Route.TH

data BackendAPI :: * -> * where
  BackendAPI_Ping :: BackendAPI ()
  
data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_API :: BackendRoute (Maybe (R BackendAPI))
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Ping :: FrontendRoute ()
  FrontendRoute_Home :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.
deriving instance Show (FrontendRoute e)

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_API -> PathSegment "api" $ maybeEncoder (unitEncoder mempty) $
        pathComponentEncoder $ \case
          BackendAPI_Ping -> PathSegment "ping" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Ping -> PathSegment "ping" $ unitEncoder mempty
      FrontendRoute_Home -> PathEnd $ unitEncoder mempty


concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''BackendAPI
  , ''FrontendRoute
  ]
