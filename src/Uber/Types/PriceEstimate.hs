{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Uber.Types.PriceEstimate where

import Data.Aeson
import Data.Text         (Text)
import GHC.Generics      (Generic)
import Uber.Auth
import WebApi

data PriceEstimateParams = PriceEstimateParams
    { start_latitude :: Double
    , start_longitude :: Double
    , end_latitude :: Double
    , end_longitude :: Double
    } deriving (Show, Generic)

newtype PriceEstimates = PriceEstimates
    { prices :: [PriceEstimate]
    } deriving (Show, Generic)

data PriceEstimate = PriceEstimate
    { product_id       :: Text
    , currency_code    :: Text
    , display_name     :: Text
    , estimate         :: Text
    , low_estimate     :: Int
    , high_estimate    :: Int
    , surge_multiplier :: Double
    , duration         :: Int
    , distance         :: Double
    } deriving (Show, Generic)

instance ToParam PriceEstimateParams 'QueryParam
instance FromJSON PriceEstimates
instance FromJSON PriceEstimate
