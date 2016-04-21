{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Uber.Types.RideRequest where

import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.List         (intercalate)
import Data.Text         (Text)
import GHC.Generics      (Generic)
import Uber.Auth
import WebApi

data RideReqParams = RideReqParams
    { product_id :: Maybe Text
    , start_latitude :: Maybe Double
    , start_longitude :: Maybe Double
    , start_nickname :: Maybe Text
    , start_address :: Maybe Text
    , start_place_id :: Maybe Text
    , end_latitude :: Maybe Double
    , end_longitude :: Maybe Double
    , end_nickname :: Maybe Text
    , end_address :: Maybe Text
    , end_place_id :: Maybe Text
    , surge_confirmation_id :: Maybe Text
    , payment_method_id :: Maybe Text
    } deriving (Generic, Show)

defRideReqParams :: RideReqParams
defRideReqParams = RideReqParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 

data RideInfo = RideInfo
    { request_id :: Text
    , status :: Status
    , vehicle :: Maybe Vehicle
    , driver :: Maybe Driver
    , location :: Maybe Location
    , eta :: Maybe Integer
    , surge_multiplier :: Maybe Double
    , pickup :: Maybe Pickup
    , destination ::Maybe Destination
    } deriving (Generic, Show)

data Status = Processing
            | NoDriversAvailable
            | Accepted
            | Arriving
            | InProgress
            | DriverCanceled
            | RiderCanceled
            | Completed
            deriving (Generic, Show)

data Vehicle = Vehicle
    { make :: Text
    , model :: Text
    , license_plate :: Text
    , picture_url :: Text
    } deriving (Generic, Show)

data Driver = Driver
    { d_phone_number :: Text
    , d_rating :: Double
    , d_picture_url :: Text
    , d_name :: Text
    } deriving (Generic, Show)

data Location = Location
    { latitude :: Double
    , longitude :: Double
    , bearing :: Int
    } deriving (Generic, Show)

data Pickup = Pickup
    { p_latitude :: Double
    , p_longitude :: Double
    , p_eta :: Double
    } deriving (Generic, Show)

data Destination = Destination
    { d_latitude :: Double
    , d_longitude :: Double
    , d_eta :: Maybe Double
    } deriving (Generic, Show)

data RidePatchParams = RidePatchParams
    { r_end_latitude  :: Maybe Double
    , r_end_longitude :: Maybe Double
    , r_end_address   :: Maybe Text
    , r_end_nickname  :: Maybe Text
    , r_end_place_id  :: Maybe Text
    } deriving (Generic, Show)

data RideEstimate = RideEstimate
    { pickup_estimate :: Int
    , price :: Price
    , trip :: Maybe Trip
    } deriving (Generic, Show)

data Price = Price
    { p_surge_multiplier :: Double
    , p_surge_confirmation_id :: Maybe Text
    , p_surge_confirmation_href :: Maybe Text
    , p_minimum :: Int
    , p_display :: Maybe Text
    , p_low_estimate :: Maybe Int
    , p_high_estimate :: Maybe Int
    , p_currency_code :: Text
    } deriving (Generic, Show)

data Trip = Trip
    { distance_estimate :: Double
    , distance_unit :: Text
    , duration_estimate :: Int
    } deriving (Generic, Show)

data ErrorResponse = ErrorResponse
    { meta :: SurgeError
    , errors :: [UberError]
    } deriving (Generic, Show)

data UberError = UberError
    { u_status :: Int
    , u_code :: Text
    , u_title :: Text
    } deriving (Generic, Show)

data SurgeError = SurgeError
    { surge_confirmation :: SurgeConfirmation
    } deriving (Generic, Show)

data SurgeConfirmation = SurgeConfirmation
    { s_href :: Text
    , s_surge_confirmation_id :: Text
    , s_multiplier :: Double
    , s_expires_at :: Integer
    } deriving (Generic, Show)

instance ToJSON RideReqParams where
instance FromJSON RideInfo where
instance FromJSON Vehicle where
instance FromJSON Location where
instance FromJSON RideEstimate where
instance FromJSON Trip where
instance FromJSON ErrorResponse where
instance FromJSON SurgeError where
instance FromJSON Price where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }
instance FromJSON Driver where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }
instance FromJSON Pickup where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }
instance FromJSON Destination where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }
instance FromJSON SurgeConfirmation where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }
instance FromJSON UberError where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }
instance FromJSON Status where
    parseJSON = genericParseJSON defaultOptions { allNullaryToStringTag = True
                                                , constructorTagModifier = toUnderscores
                                                }
instance ToJSON RidePatchParams where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 2 }

splitOnUpper :: String -> [String]
splitOnUpper [] = []
splitOnUpper s = let (ys, zs) = fn (lowerHead s) in (ys : splitOnUpper zs)
  where
    fn [] = ([], [])
    fn (x:xs) = if isUpper x
                    then ([], toLower x : xs)
                    else let (ys, zs) = fn xs in (x:ys, zs)
    lowerHead [] = []
    lowerHead (x:xs) = toLower x : xs

toUnderscores :: String -> String
toUnderscores = map toLower . intercalate "_" . splitOnUpper
