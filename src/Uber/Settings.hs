{-# LANGUAGE OverloadedStrings #-}
module Uber.Settings
    ( defSettings
    , sandboxSettings
    , auth
    , Settings
    , toClientSettings
    ) where

import Data.Text         (unpack, Text)
import Uber.Auth
import WebApi

data Settings = Settings
    { url     :: Text
    , auth    :: Token
    , manager :: Manager
    }

sandboxSettings :: Manager -> Settings
sandboxSettings mngr = Settings "https://sandbox-api.uber.com" (Server $ ServerToken "") mngr

defSettings :: Manager -> Settings
defSettings mngr = Settings "https://api.uber.com" (Server (ServerToken "")) mngr

toClientSettings :: Settings -> ClientSettings
toClientSettings (Settings u _ m) = ClientSettings (unpack u) m
