{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Uber.Contract where

import WebApi
import Data.Proxy
import Data.Text.Encoding
import Data.Text (Text)
import Uber.Auth
import Uber.Types.Product
import Uber.Types.PriceEstimate
import Uber.Types.TimeEstimate
import Uber.Types.History
import Uber.Types.UserInfo
import Uber.Types.RideRequest
import Uber.Types.Misc
import Uber.Types.Reminder

data UberAPI 

type ProductsR        = Static "products"
type ProductDetailsR  = "products" :/ Text
type PriceEstimateR   = "estimates" :/ "price"
type TimeEstimateR    = "estimates" :/ "time"
type HistoryR         = Static "history"
type UserInfoR        = Static "me"
type RequestRideR     = Static "requests"
type CurrentRequestR  = "requests" :/ "current"
type RideR            = "requests" :/ Text
type RideEstimateR    = "requests" :/ "estimate"
type TrackingMapR     = "requests" :/ Text :/ "map"
type ReceiptR         = "requests" :/ Text :/ "receipts"
type PlaceR           = "places" :/ Text
type PaymentMethodR   = Static "payment-methods"
type CreateReminderR  = Static "reminders"
type ReminderR        = "reminders" :/ Text

instance WebApi UberAPI where
    type Apis UberAPI =
        '[ Route '[GET                ] ProductsR
         , Route '[GET                ] ProductDetailsR
         , Route '[GET                ] PriceEstimateR
         , Route '[GET                ] TimeEstimateR
         , Route '[GET                ] HistoryR
         , Route '[GET                ] UserInfoR
         , Route '[POST               ] RequestRideR
         , Route '[GET, PATCH, DELETE ] CurrentRequestR
         , Route '[GET, PATCH, DELETE ] RideR
         , Route '[POST               ] RideEstimateR
         , Route '[GET                ] TrackingMapR
         , Route '[GET                ] ReceiptR
         , Route '[GET, PUT           ] PlaceR
         , Route '[GET                ] PaymentMethodR
         , Route '[POST               ] CreateReminderR
         , Route '[GET, PATCH, DELETE ] ReminderR
         ]

instance ApiContract UberAPI GET ProductsR where
    type HeaderIn   GET ProductsR = Token
    type QueryParam GET ProductsR = LatLng
    type ApiOut     GET ProductsR = Products

instance ApiContract UberAPI GET ProductDetailsR where
    type HeaderIn  GET ProductDetailsR = Token
    type PathParam GET ProductDetailsR = ProdId
    type ApiOut    GET ProductDetailsR = Product

instance ApiContract UberAPI GET PriceEstimateR where
    type HeaderIn   GET PriceEstimateR = Token
    type QueryParam GET PriceEstimateR = PriceEstimateParams
    type ApiOut     GET PriceEstimateR = PriceEstimates

instance ApiContract UberAPI GET TimeEstimateR where
    type HeaderIn   GET TimeEstimateR = Token
    type QueryParam GET TimeEstimateR = TimeEstimateParams
    type ApiOut     GET TimeEstimateR = TimeEstimates

instance ApiContract UberAPI GET HistoryR where
    type HeaderIn   GET HistoryR = OAuthToken
    type QueryParam GET HistoryR = HistoryParams
    type ApiOut     GET HistoryR = HistoryResp

instance ApiContract UberAPI GET UserInfoR where
    type HeaderIn   GET UserInfoR = OAuthToken
    type ApiOut     GET UserInfoR = UserInfo

instance ApiContract UberAPI POST RequestRideR where
    type HeaderIn    POST RequestRideR = OAuthToken
    type RequestBody POST RequestRideR = '[RideReqParams]
    type ApiOut      POST RequestRideR = RideInfo
    type ApiErr      POST RequestRideR = ErrorResponse

instance ApiContract UberAPI GET CurrentRequestR where
    type HeaderIn GET CurrentRequestR = OAuthToken
    type ApiOut   GET CurrentRequestR = RideInfo

instance ApiContract UberAPI PATCH CurrentRequestR where
    type HeaderIn    PATCH CurrentRequestR = OAuthToken
    type RequestBody PATCH CurrentRequestR = '[RidePatchParams]
    type ApiOut      PATCH CurrentRequestR = HTMLText
    type ApiErr      PATCH CurrentRequestR = HTMLText
    type ContentTypes PATCH CurrentRequestR = '[HTML]

instance ApiContract UberAPI DELETE CurrentRequestR where
    type HeaderIn     DELETE CurrentRequestR = OAuthToken
    type ApiOut       DELETE CurrentRequestR = HTMLText
    type ApiErr       DELETE CurrentRequestR = HTMLText
    type ContentTypes DELETE CurrentRequestR = '[HTML]

instance ApiContract UberAPI POST RideEstimateR where
    type HeaderIn    POST RideEstimateR = OAuthToken
    type RequestBody POST RideEstimateR = '[RideReqParams]
    type ApiOut      POST RideEstimateR = RideEstimate

instance ApiContract UberAPI GET RideR where
    type HeaderIn  GET RideR = OAuthToken
    type PathParam GET RideR = Text
    type ApiOut    GET RideR = RideInfo

instance ApiContract UberAPI PATCH RideR where
    type HeaderIn    PATCH RideR = OAuthToken
    type PathParam   PATCH RideR = Text
    type RequestBody PATCH RideR = '[RidePatchParams]
    type ApiOut      PATCH RideR = HTMLText
    type ApiErr      PATCH RideR = HTMLText
    type ContentTypes PATCH RideR = '[HTML]

instance ApiContract UberAPI DELETE RideR where
    type HeaderIn  DELETE RideR = OAuthToken
    type PathParam DELETE RideR = Text
    type ApiOut      DELETE RideR = HTMLText
    type ApiErr      DELETE RideR = HTMLText
    type ContentTypes DELETE RideR = '[HTML]

instance ApiContract UberAPI GET TrackingMapR where
    type HeaderIn  GET TrackingMapR = OAuthToken
    type PathParam GET TrackingMapR = Text
    type ApiOut    GET TrackingMapR = TrackingMap

instance ApiContract UberAPI GET ReceiptR where
    type HeaderIn  GET ReceiptR = OAuthToken
    type PathParam GET ReceiptR = Text
    type ApiOut    GET ReceiptR = Receipt
    
instance ApiContract UberAPI GET PlaceR where
    type HeaderIn  GET PlaceR = OAuthToken
    type PathParam GET PlaceR = Text
    type ApiOut    GET PlaceR = Address
    
instance ApiContract UberAPI PUT PlaceR where
    type HeaderIn    PUT PlaceR = OAuthToken
    type PathParam   PUT PlaceR = Text
    type RequestBody PUT PlaceR = '[Address]
    type ApiOut      PUT PlaceR = Address
    
instance ApiContract UberAPI GET PaymentMethodR where
    type HeaderIn  GET PaymentMethodR = OAuthToken
    type ApiOut    GET PaymentMethodR = PaymentMethods
    
instance ApiContract UberAPI POST CreateReminderR where
    type HeaderIn    POST CreateReminderR = ServerToken
    type RequestBody POST CreateReminderR = '[ReminderParams]
    type ApiOut      POST CreateReminderR = Reminder

instance ApiContract UberAPI GET ReminderR where
    type HeaderIn  GET ReminderR = ServerToken
    type PathParam GET ReminderR = Text
    type ApiOut    GET ReminderR = Reminder

instance ApiContract UberAPI PATCH ReminderR where
    type HeaderIn    PATCH ReminderR = ServerToken
    type PathParam   PATCH ReminderR = Text
    type RequestBody PATCH ReminderR = '[ReminderPatchParams]
    type ApiOut      PATCH ReminderR = Reminder

instance ApiContract UberAPI DELETE ReminderR where
    type HeaderIn  DELETE ReminderR = ServerToken
    type PathParam DELETE ReminderR = Text
    type ApiOut       DELETE ReminderR = HTMLText
    type ApiErr       DELETE ReminderR = HTMLText
    type ContentTypes DELETE ReminderR = '[HTML]

instance Decode HTML HTMLText where
  decode _ = (HTMLText <$>) . (decode (Proxy :: Proxy PlainText))

data HTMLText = HTMLText Text deriving Show
