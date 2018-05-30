{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Lib
    (
    ) where

import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)

import Data.Aeson
import Servant.API ((:<|>), (:>), Get, JSON)
import Servant.Server (Server, serve)

data User = User
  { name :: String
  , age :: Int
  , email :: String
  } deriving (Eq, Show, Generic)
instance ToJSON User

users1 :: [User]
users1 =
  [ User "Issac Newton" 372 "issac@newton.co.uk"
  , User "Albert Einstein" 136 "ae@mc2.org"
  ]

type UserAPI1 = "users" :> Get '[JSON] [User]

main:: IO ()
main = run 1111 $ serve (Proxy::Proxy UserAPI1) (return users1)
