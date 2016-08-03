{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Uber.Types.History where

import Data.Aeson
import Data.Aeson.Types
import Data.Text         (Text)
import GHC.Generics      (Generic)
import Uber.Auth
import WebApi

data HistoryParams = HistoryParams
    { offset :: Maybe Int
    , limit :: Maybe Int
    } deriving (Show, Generic)

data HistoryResp = HistoryResp
    { h_offset :: Int
    , h_limit :: Int
    , h_count :: Int
    , h_history :: [History]
    } deriving (Show, Generic)

data History = History
    { request_id :: Text
    , request_time :: Integer
    , product_id :: Text
    , status :: Text
    , distance :: Double
    , start_time :: Integer
    , start_city :: City
    } deriving (Show, Generic)

data City = City
    { display_name :: Text
    , latitude :: Double
    , longitude :: Double
    } deriving (Show, Generic)

instance ToParam 'QueryParam HistoryParams
instance FromJSON History where
instance FromJSON City where
instance FromJSON HistoryResp where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }
