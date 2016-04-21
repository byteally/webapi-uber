{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Text as T
import Uber
import Uber.Types.History 
import Uber.Types.Misc
import Uber.Types.PriceEstimate
import Uber.Types.Product
import Uber.Types.Reminder 
import Uber.Types.RideRequest as RR
import Uber.Types.TimeEstimate
import Uber.Types.UserInfo
import System.Environment (getEnv)
import Test.Hspec

main :: IO ()
main = do
    oauthTkn <- getEnv "UBER_OAUTH_TOKEN"
    serverTkn <- getEnv "UBER_SERVER_TOKEN"
    manager <- newManager tlsManagerSettings
    hspec $ do
        let fakeSettings = (sandboxSettings manager) { auth = OAuth (OAuthToken "FakeToken") }
            settingsWithOAuth = (sandboxSettings manager) { auth = OAuth (OAuthToken $ T.pack oauthTkn) }
            settingsWithServerToken = (sandboxSettings manager) { auth = Server (ServerToken $ T.pack serverTkn) }
        describe "getProducts: Authentication with FakeToken" $ do
            it "should fail" $ do
                resp <- getProducts fakeSettings (LatLng 13.0827 80.2707)
                resp `shouldSatisfy` (not . isSuccess)
        describe "getProducts: Authentication with OAuthToken" $ do
            shouldPass $ getProducts settingsWithOAuth (LatLng 13.0827 80.2707)
        describe "getProducts: Authentication with ServerToken" $ do
            shouldPass $ getProducts settingsWithServerToken (LatLng 13.0827 80.2707)
        describe "getProductDetails:" $ do
            shouldPass $ getProductDetails settingsWithOAuth (ProductId "c58b6622-0be7-4de1-ad60-a9c4d6f4eaa5")
        describe "getPriceEstimate:" $ do
            shouldPass $ getPriceEstimate settingsWithOAuth (PriceEstimateParams 12.9760 80.2212 13.0012 80.2565)
        describe "getTimeEstimate:" $ do
            shouldPass $ getTimeEstimate settingsWithOAuth (TimeEstimateParams 12.9760 80.2212 Nothing)
        describe "getHistory:" $ do
            shouldPass $ getHistory settingsWithOAuth (HistoryParams Nothing Nothing)
        describe "getUserInfo:" $ do
            shouldPass $ getUserInfo settingsWithOAuth
        describe "getCurrentRequest:" $ do
            shouldPass $ getCurrentRequest settingsWithOAuth
        describe "updateCurrentRequest:" $ do
            shouldPass $ updateCurrentRequest settingsWithOAuth $ RidePatchParams (Just 13.0012) (Just 80.2565) Nothing Nothing Nothing
        describe "cancelCurrentRequest:" $ do
            shouldPass $ cancelCurrentRequest settingsWithOAuth
        describe "getRideEstimate:" $ do
            shouldPass $ getRideEstimate settingsWithOAuth (defRideReqParams
                { RR.start_latitude = Just 12.9760 
                , RR.start_longitude = Just 80.2212
                })
        describe "requestARide:" $ do
            it "should pass" $ do
                resp <- requestARide settingsWithOAuth (defRideReqParams
                    { RR.start_latitude = Just 12.9760 
                    , RR.start_longitude = Just 80.2212
                    })
                case resp of
                    Success _ r _ _ -> do
                        let rid = RR.request_id r
                        res1 <- getRideStatus settingsWithOAuth rid
                        res2 <- updateRide settingsWithOAuth rid $ RidePatchParams (Just 13.0012) (Just 80.2565) Nothing Nothing Nothing
                        res3 <- cancelRide settingsWithOAuth rid
                        case (res1, res2, res3) of
                            (Success {}, Success {}, Success {}) -> return ()
                            _ -> fail "Reminder request failed!"
                    Failure {} -> fail "`requestARide` Failed"
        describe "getPaymentMethods:" $ do
            shouldPass $ getPaymentMethods settingsWithOAuth
        describe "reminder:" $ do
            it "should pass" $ do
                resp <- createReminder settingsWithServerToken $ ReminderParams 1461984150 "+919876543210" (Event 1462984150 Nothing Nothing Nothing Nothing Nothing) (Just $ TripBranding Nothing Nothing)
                case resp of
                    Success _ r _ _ -> do
                        let rid = r_reminder_id r
                        res1 <- getReminderInfo settingsWithServerToken rid
                        res2 <- updateReminder settingsWithServerToken rid $ ReminderPatchParams (Just 1461984150) (Just "+911234567890") (Just $ Event 1462984150 Nothing Nothing Nothing Nothing Nothing)
                        res3 <- deleteReminder settingsWithServerToken rid
                        case (res1, res2, res3) of
                            (Success {}, Success {}, Success {}) -> return ()
                            _ -> fail "Reminder request failed!"
                    Failure {} -> fail "`createReminder` Failed"

shouldPass :: (Show (ApiOut m r), Show (ApiErr m r)) => IO (Response m r) -> SpecWith ()
shouldPass x = do
    it "should pass" $ do
        resp <- x
        resp `shouldSatisfy` isSuccess

instance Show OtherError where
    show (OtherError e) = show e

instance (Show (ApiOut m r), Show (ApiErr m r)) => Show (Response m r) where
    show (Success _ x _ _) = show x
    show (Failure (Left x)) = show x
    show (Failure (Right x)) = show x

instance Show (ApiErr m r) => Show (ApiError m r) where
    show (ApiError _ x _ _) = show x

isSuccess :: Response m r -> Bool
isSuccess r = case r of
    Success {} -> True
    Failure {} -> False
