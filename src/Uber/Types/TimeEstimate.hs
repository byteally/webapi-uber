{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Uber.Types.TimeEstimate where

import Data.Aeson
import Data.Aeson.Types
import Data.Text         (Text)
import GHC.Generics      (Generic)
import Uber.Auth
import WebApi

data TimeEstimateParams = TimeEstimateParams
    { start_latitude :: Double
    , start_longitude :: Double
    , product_id :: Maybe Text
    } deriving (Show, Generic)

newtype TimeEstimates = TimeEstimates { times :: [TimeEstimate] }
    deriving (Show, Generic)

data TimeEstimate = TimeEstimate
    { t_product_id   :: Text
    , t_display_name :: Text
    , t_estimate     :: Int
    } deriving (Show, Generic)

instance ToParam TimeEstimateParams 'QueryParam
instance FromJSON TimeEstimates where
instance FromJSON TimeEstimate where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }

