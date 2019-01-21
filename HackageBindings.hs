{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module HackageBindings where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import qualified Data.Text as T
import qualified Data.Text.IO as T

type HackageAPI =
       "users" :> Get '[JSON] [UserSummary]
  :<|> "user" :> Capture "username" Username :> Get '[JSON] UserDetailed
  :<|> "packages" :> Get '[JSON] [Package]

type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int
  } deriving (Eq, Show)

instance FromJSON UserSummary where
  parseJSON (Object o) =
    UserSummary <$> o .: "username"
                <*> o .: "userid"
  parseJSON _ = mzero

type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid  :: Int
  , groups  :: [Group]
  } deriving (Eq, Show, Generic)

instance FromJSON UserDetailed

newtype Package = Package { packageName :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON Package

hackageAPI :: Proxy HackageAPI
hackageAPI = Proxy

getUsers :: ClientM [UserSummary]
getUser :: Username -> ClientM UserDetailed
getPackages :: ClientM [Package]
getUsers :<|> getUser :<|> getPackages = client hackageAPI

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  users <- runClientM getUsers (mkClientEnv manager' (BaseUrl Http "hackage.haskell.org" 80 ""))
  print users
