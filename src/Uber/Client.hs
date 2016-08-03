module Uber.Client where

import Data.Text
import Uber.Auth
import Uber.Contract
import Uber.Settings
import Uber.Types.Product
import Uber.Types.PriceEstimate
import Uber.Types.TimeEstimate
import Uber.Types.History
import Uber.Types.UserInfo
import Uber.Types.RideRequest
import Uber.Types.Misc
import Uber.Types.Reminder
import WebApi

-- | Returns information about the Uber products offered at a given location
getProducts :: Settings -> LatLng -> IO (Response GET ProductsR)
getProducts settings ll = do
    client (addV1 $ toClientSettings settings) $
        Request () ll () () (auth settings) () ()

-- | Returns information aboit a specific Uber product
getProductDetails :: Settings -> ProductId -> IO (Response GET ProductDetailsR)
getProductDetails settings (ProductId pid) = do
    client (addV1 $ toClientSettings settings) $
        Request (ProdId pid) () () () (auth settings) () ()

-- | Returns an estimated price range for each product offered at a given location
getPriceEstimate :: Settings -> PriceEstimateParams -> IO (Response GET PriceEstimateR)
getPriceEstimate settings pp = do
    client (addV1 $ toClientSettings settings) $
        Request () pp () () (auth settings) () ()

-- | Returns ETAs for all products offered at a given location
getTimeEstimate :: Settings -> TimeEstimateParams -> IO (Response GET TimeEstimateR)
getTimeEstimate settings tp = do
    client (addV1 $ toClientSettings settings) $
        Request () tp () () (auth settings) () ()

-- | Returns data about a user’s activity with Uber
getHistory :: Settings -> HistoryParams -> IO (Response GET HistoryR)
getHistory settings hp = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "getHistory: Requires OAuth token"
    client (addV12 $ toClientSettings settings) $
        Request () hp () () tkn () ()

-- | Returns information about the Uber user that has authorized with the application
getUserInfo :: Settings -> IO (Response GET UserInfoR)
getUserInfo settings = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "getUserInfo: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request () () () () tkn () ()

-- | Requests a ride on behalf of an Uber user
requestARide :: Settings -> RideReqParams -> IO (Response POST RequestRideR)
requestARide settings rp = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "requestARide: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request () () () () tkn () rp

-- | Returns the real-time details for an ongoing trip
getCurrentRequest :: Settings -> IO (Response GET CurrentRequestR)
getCurrentRequest settings = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "getCurrentRequest: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request () () () () tkn () ()

-- | Updates an ongoing request’s destination
updateCurrentRequest :: Settings -> RidePatchParams -> IO (Response PATCH CurrentRequestR)
updateCurrentRequest settings rp = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "updateCurrentRequest: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request () () () () tkn () rp

-- | Cancels the user's current trip
cancelCurrentRequest :: Settings -> IO (Response DELETE CurrentRequestR)
cancelCurrentRequest settings = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "cancelCurrentRequest: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request () () () () tkn () ()

-- | Gets a ride's estimates given the desired product, start, and end locations
getRideEstimate :: Settings -> RideReqParams -> IO (Response POST RideEstimateR)
getRideEstimate settings rp = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "getRideEstimate: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request () () () () tkn () rp

type RideId = Text
type PlaceId = Text
type ReminderId = Text

-- | Gets the status of an ongoing or completed trip
getRideStatus :: Settings -> RideId -> IO (Response GET RideR)
getRideStatus settings rid = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "getRideStatus: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request rid () () () tkn () ()

-- | Cancels an ongoing Request on behalf of a rider
cancelRide :: Settings -> RideId -> IO (Response GET RideR)
cancelRide settings rid = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "cancelRide: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request rid () () () tkn () ()

-- | Updates an ongoing request’s destination
updateRide :: Settings -> RideId -> RidePatchParams -> IO (Response PATCH RideR)
updateRide settings rid rp = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "updateRide: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request rid () () () tkn () rp

-- | Gets a map for an accepted Request
getTrackingMap :: Settings -> RideId -> IO (Response GET TrackingMapR)
getTrackingMap settings rid = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "getTrackingMap: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request rid () () () tkn () ()

-- | Gets the receipt information of the completed request
getReceipt :: Settings -> RideId -> IO (Response GET ReceiptR)
getReceipt settings rid = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "getReceipt: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request rid () () () tkn () ()

-- | Retrieves home and work addresses from an Uber user's profile
getAddress :: Settings -> PlaceId -> IO (Response GET PlaceR)
getAddress settings pid = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "getAddress: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request pid () () () tkn () ()

-- | Updates home and work addresses for an Uber user's profile
updateAddress :: Settings -> PlaceId -> Address -> IO (Response PUT PlaceR)
updateAddress settings pid addr = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "updateAddress: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request pid () () () tkn () addr

-- | Retrievs the list of user’s available payment methods
getPaymentMethods :: Settings -> IO (Response GET PaymentMethodR)
getPaymentMethods settings = do
    let tkn = case auth settings of
                OAuth tkn -> tkn
                _         -> error "getPaymentMethods: Requires OAuth token"
    client (addV1 $ toClientSettings settings) $
        Request () () () () tkn () ()

-- | Sets a reminder for a future trip
createReminder :: Settings -> ReminderParams -> IO (Response POST CreateReminderR)
createReminder settings rp = do
    let tkn = case auth settings of
                Server tkn -> tkn
                _          -> error "createReminder: Requires Server token"
    client (addV1 $ toClientSettings settings) $
        Request () () () () tkn () rp

-- | Gets the status of an existing ride reminder
getReminderInfo :: Settings -> ReminderId -> IO (Response GET ReminderR)
getReminderInfo settings rid = do
    let tkn = case auth settings of
                Server tkn -> tkn
                _          -> error "getReminderInfo: Requires Server token"
    client (addV1 $ toClientSettings settings) $
        Request rid () () () tkn () ()

-- | Removes any reminder in the pending state from being sent
deleteReminder :: Settings -> ReminderId -> IO (Response DELETE ReminderR)
deleteReminder settings rid = do
    let tkn = case auth settings of
                Server tkn -> tkn
                _          -> error "deleteReminder: Requires Server token"
    client (addV1 $ toClientSettings settings) $
        Request rid () () () tkn () ()

-- | Updates an existing reminder
updateReminder :: Settings -> ReminderId -> ReminderPatchParams -> IO (Response PATCH ReminderR)
updateReminder settings rid rp = do
    let tkn = case auth settings of
                Server tkn -> tkn
                _          -> error "updateReminder: Requires Server token"
    client (addV1 $ toClientSettings settings) $
        Request rid () () () tkn () rp

addV1 :: ClientSettings -> ClientSettings
addV1 settings = settings { baseUrl = baseUrl settings ++ "/v1" }

addV12 :: ClientSettings -> ClientSettings
addV12 settings = settings { baseUrl = baseUrl settings ++ "/v1.2" }
