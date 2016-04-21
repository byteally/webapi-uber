{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Uber.Types.Misc where

import Data.Aeson
import Data.Aeson.Types
import Data.Text              (Text)
import GHC.Generics           (Generic)
import Uber.Types.RideRequest (toUnderscores)
import WebApi

data TrackingMap = TrackingMap
    { t_request_id :: Text
    , t_href :: Text
    } deriving (Generic, Show)

data Receipt = Receipt
    { request_id :: Text
    , charges :: [Charge]
    , surge_charge  :: Maybe Charge
    , charge_adjustments :: [Charge]
    , normal_fare :: Text
    , subtotal :: Text
    , total_charged :: Text
    , total_owed :: Maybe Double
    , currency_code :: Text
    , duration :: Text
    , distance :: Text
    , distance_label :: Text
    } deriving (Generic, Show)

data Charge = Charge
    { name :: Text
    , amount :: Double
    } deriving (Generic, Show)

data Address = Address
    { address :: Text
    } deriving (Generic, Show)

newtype PaymentMethods = PaymentMethods 
    { payment_methods :: [PaymentMethod]
    } deriving (Generic, Show)

data PaymentMethod = PaymentMethod
    { p_payment_method_id :: Text
    , p_type :: PaymentType
    , p_description :: Maybe Text
    , p_last_used :: Maybe Text
    } deriving (Generic, Show)

data PaymentType
    = AirtelMoney
    | Alipay
    | ApplePay
    | AmericanExpress
    | BaiduWallet
    | BusinessAccount
    | Cash
    | Discover
    | GoogleWallet
    | Jcb
    | Lianlian
    | Maestro
    | Mastercard
    | Paypal
    | Paytm
    | Ucharge
    | Unionpay
    | Unknown
    | Visa
    | Zaakpay
    deriving (Generic, Show)

instance FromJSON Receipt where
instance FromJSON Charge where
instance FromJSON Address where
instance FromJSON PaymentMethods where
instance ToJSON Address where
instance FromJSON PaymentMethod where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }
instance FromJSON TrackingMap where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }
instance FromJSON PaymentType where
    parseJSON = genericParseJSON defaultOptions { allNullaryToStringTag = True
                                                , constructorTagModifier = toUnderscores
                                                }
