{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Uber.Types.Reminder where

import Data.Aeson
import Data.Aeson.Types
import Data.Char                      (toLower)
import Data.Text                      (Text)
import GHC.Generics                   (Generic)
import WebApi

data ReminderParams = ReminderParams
    { reminder_time :: Integer
    , phone_number :: Text
    , event :: Event
    , trip_branding :: Maybe TripBranding
    } deriving (Generic, Show)

data Event = Event
    { time :: Integer
    , name :: Maybe Text
    , location :: Maybe Text
    , latitude :: Maybe Double
    , longitude :: Maybe Double
    , product_id :: Maybe Text
    } deriving (Generic, Show)

data TripBranding = TripBranding
    { link_text :: Maybe Text
    , partner_deeplink :: Maybe Text
    } deriving (Generic, Show)

data Reminder = Reminder
    { r_event :: Event
    --, r_product_id :: Maybe Text
    , r_reminder_id :: Text
    , r_reminder_time :: Integer
    , r_reminder_status :: ReminderStatus
    , r_trip_branding :: Maybe TripBranding
    } deriving (Generic, Show)

data ReminderStatus = Pending | Sent
    deriving (Generic, Show)

data ReminderPatchParams = ReminderPatchParams
    { p_reminder_time :: Maybe Int
    , p_phone_number :: Maybe Text
    , p_event :: Maybe Event
    --, p_trip_branding :: Maybe TripBranding
    } deriving (Generic, Show)


instance ToJSON ReminderParams
instance FromJSON Event
instance FromJSON TripBranding
instance ToJSON Event
instance ToJSON TripBranding
instance ToJSON ReminderPatchParams where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 2 }
instance FromJSON Reminder where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }
instance FromJSON ReminderStatus where
    parseJSON = genericParseJSON defaultOptions { allNullaryToStringTag = True
                                                , constructorTagModifier = map toLower
                                                }
