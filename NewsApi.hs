{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module NewsApi where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

data Source = Source
  { id :: Int
  , name :: Text
  } deriving (Show, Generic)

instance FromJSON Source

data Article = Article
  { source :: Source
  , author :: Text
  , title :: Text
  , description :: Text
  , url :: Text
  , urlToImage :: Text
  , publishedAt :: Text
  , content :: Text
  } deriving (Show, Generic)

instance FromJSON Article

data NewsResult = NewsResult
  { status :: Text
  , totalResults :: Int
  , articles :: [Article]
  } deriving (Show, Generic)

instance FromJSON NewsResult

data SourceInfo = SourceInfo
  { sourceId :: Text
  , sourceName :: Text
  , sourceDescription :: Text
  , sourceUrl :: Text
  , sourceCategory :: Text
  , sourceLanguage :: Text
  , sourceCountry :: Text
  } deriving (Show, Generic)

instance FromJSON SourceInfo where
  parseJSON (Object o) =
    SourceInfo <$> o .: "id"
               <*> o .: "name"
               <*> o .: "description"
               <*> o .: "url"
               <*> o .: "category"
               <*> o .: "language"
               <*> o .: "country"

data SourcesResult = SourcesResult
  { sourceStatus :: Text
  , sourceInfo :: [SourceInfo]
  } deriving (Show, Generic)

instance FromJSON SourcesResult where
  parseJSON (Object o) =
    SourcesResult <$> o .: "status"
                  <*> o .: "sources"

type NewsAPI = 
       "top-headlines" :>
  QueryParam "country" Text :>
  QueryParam "category" Text :>
  QueryParam "sources" Text :>
  QueryParam "q" Text :>
  QueryParam "pageSize" Int :>
  QueryParam "page" Int :>
  QueryParam "apiKey" Text :>
  Get '[JSON] NewsResult
  :<|> "everything" :>
  QueryParam "q" Text :>
  QueryParam "sources" Text :>
  QueryParam "domains" Text :>
  QueryParam "excludeDomains" Text :>
  QueryParam "from" Text :>
  QueryParam "to" Text :>
  QueryParam "langauge" Text :>
  QueryParam "sortBy" Text :>
  QueryParam "pageSize" Int :>
  QueryParam "page" Int :>
  QueryParam "apiKey" Text :>
  Get '[JSON] NewsResult
  :<|> "sources" :>
  QueryParam "category" Text :>
  QueryParam "language" Text :>
  QueryParam "country" Text :>
  QueryParam "apiKey" Text :>
  Get '[JSON] SourcesResult

topheadlines :: 
     Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> ClientM NewsResult

everything ::
     Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> ClientM NewsResult

sources ::
     Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM SourcesResult

api :: Proxy NewsAPI
api = Proxy

topheadlines :<|> everything :<|> sources = client api

query3 = sources Nothing Nothing Nothing (Just "90a38fab85c440fa88521e0789248f83")

query2 = everything (Just "bitcoin") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "90a38fab85c440fa88521e0789248f83")

query = topheadlines (Just "us") Nothing Nothing Nothing Nothing Nothing (Just "90a38fab85c440fa88521e0789248f83")


run3 :: IO ()
run3 = do
  manager' <- newManager defaultManagerSettings
  burl <- parseBaseUrl "http://newsapi.org/v2"
  res1 <- runClientM query (mkClientEnv manager' burl)
  res2 <- runClientM query2 (mkClientEnv manager' burl)
  res3 <- runClientM query3 (mkClientEnv manager' burl)
  print res3
