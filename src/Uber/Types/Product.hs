{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Uber.Types.Product where

import Data.Aeson
import Data.Text         (Text)
import GHC.Generics      (Generic)
import Uber.Auth
import WebApi

data LatLng = LatLng
    { latitude  :: Double
    , longitude :: Double
    } deriving (Show, Generic)

newtype Products = Products { products :: [Product] }
    deriving (Show, Generic)

data Product = Product
    { product_id        :: Text
    , short_description :: Maybe Text
    , description       :: Text
    , display_name      :: Text
    , capacity          :: Int
    , image             :: Text
    , price_details     :: PriceDetails
    } deriving (Show, Generic)

data PriceDetails = PriceDetails
    { base              :: Double
    , minimum           :: Double
    , cost_per_minute   :: Double
    , cost_per_distance :: Double
    , distance_unit     :: Text
    , cancellation_fee  :: Double
    , currency_code     :: Text
    , service_fees      :: [AdditionalFee]
    } deriving (Show, Generic)

data AdditionalFee = AdditionalFee
    { name :: Text
    , fee  :: Double
    } deriving (Show, Generic)

newtype ProdId = ProdId
    { pid :: Text
    } deriving (Generic)

data ProductId = ProductId Text

instance ToParam 'QueryParam LatLng
instance ToParam 'PathParam ProdId
instance FromJSON Products
instance FromJSON Product
instance FromJSON PriceDetails
instance FromJSON AdditionalFee
