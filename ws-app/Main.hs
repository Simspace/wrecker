{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Control.Exception (throw, catch, SomeException)
import Control.Lens (preview, view, elemOf, (&), (.~))
import Data.Aeson (encode, Value(Number, String))
import Data.Aeson.Lens (key)
import Data.Aeson.QQ
import Data.Scientific (Scientific)
-- import Data.Aeson.TH (deriveFromJSON)
import Data.CaseInsensitive (mk)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Map as Map
import qualified Network.HTTP.Client as HTTP
import Network.Wreq (responseHeader, responseCookie, defaults, manager, postWith)
import Network.Socket (PortNumber)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Wrecker
import qualified Network.Websockets.Wrecker as WSW
import qualified Network.Wreq.Wrecker as WRW

data AppConfig = AppConfig
               { appHttpScheme :: String
               , appHost :: String
               , appPort :: PortNumber
               , appEventKey :: String
               , appVcnId :: String
               , appSessionId :: String
               , appEmail :: String
               , appPassword :: String
               }

hasSequenceNumber :: Scientific -> a -> Value -> Bool
hasSequenceNumber s _ = elemOf (key (pack "sequence")) (Number s)

isTrackerFrameType :: String -> a -> Value -> Bool
isTrackerFrameType frameType _ b = elemOf (key (pack "app")) (String . pack $ "tracker") b && elemOf (key (pack "payload") . key (pack "frameType")) (String . pack $ frameType) b

qaApp :: AppConfig
qaApp = AppConfig "https://" "qa.simspace.com" 443 "fde4b98a-b232-407e-918d-251ca9fc677a" "57d86bd50f00006c99b4396e" "571fdb6b010000a509506745" "matt@simspace.com" "krPAbTpK8th6"

localApp :: AppConfig
localApp = AppConfig "http://" "localhost" 9050 "demo_event" "56c48a82d5d111e200593c6c" "56bdfd1bd5d111a200df81b5" "" ""

testScript :: Maybe HTTP.Cookie -> AppConfig -> Environment -> IO ()
testScript mCookie AppConfig{..} env = do
  let httpPath = appHttpScheme ++ appHost ++ ":" ++ show appPort
      wsPath = "/api/websocket"
      flippedWithWebSockets headers = \ f -> WSW.withWebSockets f appHost appPort headers wsPath env

  cookie <- WRW.withWreqSettings (recorder env) (Just (HTTP.createCookieJar (maybe [] (:[]) mCookie))) (WRW.defaultManagerSettings (context env)) $ \ sess -> do
    WRW.get sess $ appHttpScheme ++ appHost ++ "/index.html"
    -- TODO look at HTML and pull JS/CSS
    WRW.post sess (appHttpScheme ++ appHost ++ "/api/portal/login") (encode . Map.fromList $ [("email", appEmail), ("password", appPassword)])
    cookie <- view (responseHeader (mk . encodeUtf8 . pack $ "Set-Cookie")) <$> (WRW.get sess $ httpPath ++ "/api/portal/me")
    WRW.get sess $ httpPath ++ "/api/range/range/vcns/me"
    WRW.get sess $ httpPath ++ "/api/range/range/vcns/" ++ appVcnId
    WRW.get sess $ httpPath ++ "/api/range/range/vcns/" ++ appVcnId ++ "/listVMs?withConsoles=true"
    WRW.get sess $ httpPath ++ "/api/range/range/vcns/" ++ appVcnId ++ "/service/fileTransfer/upload"
    WRW.get sess $ httpPath ++ "/api/range/range/vcns/" ++ appVcnId ++ "/service/FileTransfer"
    WRW.post sess (httpPath ++ "/api/range/range/vcns/" ++ appVcnId ++ "/logs") [aesonQQ| {} |]
    return cookie
  flip catch (\ (e :: SomeException) -> print e >> throw e) (flippedWithWebSockets [(mk . encodeUtf8 . pack $ "Cookie", cookie), (mk . encodeUtf8 . pack $ "Origin", encodeUtf8 . pack $ "https://qa.simspace.com")] $ \ conn -> do
    WSW.sendAndReceiveMessage (hasSequenceNumber 1) conn "latency" [aesonQQ| {frameType: "portal", portalFrameType: "latency", timing: "foo", sequence: 1} |]
    WSW.sendAndReceiveMessage (hasSequenceNumber 2) conn "setEvent" [aesonQQ| {frameType: "portal", key: #{appEventKey}, portalFrameType: "setEvent", sequence: 2} |]
    WSW.sendAndReceiveMessage (isTrackerFrameType "open") conn "Tracker Connect" [aesonQQ| {frameType: "connect", app: "tracker", sequence: 3} |]
    WSW.sendAndReceiveMessage (isTrackerFrameType "sessionOpen") conn "setSession" [aesonQQ| {sequence: 4, app: "tracker", frameType: "fromClient", payload: {category: "event", frameType: "setSession", sessionId: #{appSessionId} } }|]
    )
  return ()

benchmarks :: IO [(String, Environment -> IO ())]
benchmarks = do
  -- Create a TLS context once
  mnger <- HTTP.newManager tlsManagerSettings { HTTP.managerResponseTimeout = HTTP.responseTimeoutNone }
  let app@AppConfig { .. } = qaApp
      opts = defaults & manager .~ Right mnger

  preview (responseCookie (encodeUtf8 . pack $ "_SESSION")) <$> (postWith opts (appHttpScheme ++ appHost ++ "/api/portal/login") (encode . Map.fromList $ [("email", appEmail), ("password", appPassword)]))
  return [("test0", testScript Nothing app)]

main :: IO ()
main = defaultMain =<< benchmarks
