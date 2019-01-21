{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ZillowClient where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)
import Xmlbf
import Servant.XML

import qualified Servant.Client.Streaming as S

host :: BaseUrl
host = BaseUrl Https "http://www.zillow.com/webservice/GetSearchResults.htm" 443 ""

type ZId = Text
type Address = Text
type CityStateOrZip = Text

type GetSearchResultsAPI =
     "webservice" :>
     "GetSearchResults.htm" :>
     QueryParam "zws-id" ZId :>
     QueryParam "address" Address :>
     QueryParam "citystatezip" CityStateOrZip :>
     Get '[XML] Listing

data Listing = Listing
  { listingZpid :: Text
  , listingLinks :: Text
  , listingAddress :: Text
  , listingZestimate :: Text
  , listingLocalRealEstate :: Text
  } deriving (Eq, Show, Generic)

--instance FromXml Listing
instance Xmlbf.FromXml Listing where
  fromXml = Xmlbf.pElement "result" $ Listing
    <$> Xmlbf.pElement "zpid" Xmlbf.pText
    <*> Xmlbf.pElement "links" Xmlbf.pText
    <*> Xmlbf.pElement "address" Xmlbf.pText
    <*> Xmlbf.pElement "zestimate" Xmlbf.pText
    <*> Xmlbf.pElement "localRealEstate" Xmlbf.pText

instance FromJSON Listing
{-
instance FromJSON Listing where
  fromJSON (Object o) =
    Listing <$> o .: "zpid"
            <*> o .: "links"
            <*> o .: "address"
            <*> o .: "zestimate"
            <*> o .: "localRealEstate"
  fromJSON _ = mzero
-}

getSearchResultsAPI :: Proxy GetSearchResultsAPI
getSearchResultsAPI = Proxy

--getResults :: ZId -> Address -> CityStateOrZip -> ClientM Listing

getResults = client getSearchResultsAPI

zid :: Maybe ZId
zid = Just "X1-ZWz18611u72xhn_9hc6e"

address :: Maybe Address
address = Just "15 Farm St"

cityStateOrZip :: Maybe CityStateOrZip
cityStateOrZip = Just "01002"

run2 :: IO ()
run2 = do
  manager' <- newManager defaultManagerSettings
  results <- runClientM (getResults zid address cityStateOrZip) (mkClientEnv manager' (BaseUrl Http "http://www.zillow.com/webservice/GetSearchResults.htm" 80 ""))
  print results
