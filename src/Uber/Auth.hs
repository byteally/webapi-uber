{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Uber.Auth
    ( Token (..)
    , ServerToken (..)
    , OAuthToken (..)
    ) where

import           Data.Monoid  ((<>))
import           Data.Text
import           GHC.Generics (Generic)
import           WebApi

data AuthHeader = AuthHeader
    { authorization :: Text
    } deriving Generic

-- | Data type used to store Server Token
newtype ServerToken = ServerToken Text
    deriving Show

-- | Data type used to store OAuth Token
newtype OAuthToken = OAuthToken Text
    deriving Show

-- | Data type used to store either Server or OAuth token
data Token = Server ServerToken
           | OAuth  OAuthToken
           deriving Show

class FromToken a where
    fromToken :: a -> Text

instance FromToken ServerToken where
    fromToken (ServerToken x) = "Token " <> x

instance FromToken OAuthToken where
    fromToken (OAuthToken x) = "Bearer " <> x

instance FromToken Token where
    fromToken (Server x) = fromToken x
    fromToken (OAuth x)  = fromToken x

instance ToHeader AuthHeader
instance ToHeader ServerToken where
    toHeader = toHeader . AuthHeader . fromToken
instance ToHeader OAuthToken where
    toHeader = toHeader . AuthHeader . fromToken
instance ToHeader Token where
    toHeader = toHeader . AuthHeader . fromToken
