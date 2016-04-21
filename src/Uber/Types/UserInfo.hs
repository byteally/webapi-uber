{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Uber.Types.UserInfo where

import Data.Aeson
import Data.Aeson.Types
import Data.Text         (Text)
import GHC.Generics      (Generic)
import Uber.Auth
import WebApi

data UserInfo = UserInfo
    { first_name :: Text
    , last_name :: Text
    , email :: Text
    , mobile_verified :: Bool
    , promo_code :: Text
    , uuid :: Text
    } deriving (Generic, Show)

instance FromJSON UserInfo
