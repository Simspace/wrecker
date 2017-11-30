{-# LANGUAGE RecordWildCards, RankNTypes #-}

module Network.Websockets.Wrecker
    ( WSConnection
    , WithMessageId (messageId)
    , withRecordFunction
    -- * Session Creation
    , withWebSockets
    -- * WS Methods
    , sendAndReceiveMessage
    , sendAndReceiveMessageWithId
    ) where

import Control.Exception (finally) --fromException, handle, throwIO)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as L
import Network.Connection
      ( ConnectionParams(ConnectionParams), TLSSettings(TLSSettingsSimple)
      , connectionGetChunk, connectionPut, connectionClose, connectTo)
import Network (PortNumber)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import Wrecker

class WithMessageId a where
  messageId :: a -> Int

{-| An opaque type created by 'withWreq', 'withWreqNoCookies',
    or 'withWreqSettings'. All HTTP calls require a 'Session'.
-}
data WSConnection = WSConnection
    { cConnection :: WS.Connection
    , cRecorder :: Recorder
    , cRecord :: forall a. Recorder -> String -> IO a -> IO a
      -- ^ A custom function to record the time of of executing the IO action
      --   By default, it will use 'Wrecker.Recorder.record'
    }

{- | Create 'ManagerSettings' with no timeout using a shared TLS
     'ConnectionContext'
-}
-- defaultManagerSettings :: ConnectionContext -> HTTP.ManagerSettings
-- defaultManagerSettings context =
--     (TLS.mkManagerSettingsContext (Just context) def Nothing)
--     {HTTP.managerResponseTimeout = HTTP.responseTimeoutNone}

-- | Create a 'Session' using the 'wrecker' 'Environment', passing it to the
--   given function.  The 'Session' will no longer be valid after that
--   function returns.
--
-- This session manages cookies and uses default session manager
-- configuration.
-- withWebsockets :: (Connection -> IO a) -> Environment -> IO a
-- withWebsockets f env =
    -- withWreqSettings
        -- (recorder env)
        -- (Just (HTTP.createCookieJar []))
        -- (defaultManagerSettings (context env))
        -- f

withWebSockets :: (WSConnection -> IO a) -> String -> PortNumber -> WS.Headers -> String -> Environment -> IO a
withWebSockets f host port headers path env = do
  -- Disable cert checking, should be an option
  let tlsSettings = TLSSettingsSimple True False False
      connectionParams = ConnectionParams host port (Just tlsSettings) Nothing
  conn <- connectTo (context env) connectionParams
  stream <- WS.makeStream (Just <$> connectionGetChunk conn) (maybe (pure ()) (connectionPut conn . L.toStrict))
  -- Failing, likely due to headers?
  WS.runClientWithRespStream stream host path WS.defaultConnectionOptions headers (\ cn _ _ -> finally (f $ WSConnection cn (recorder env) record) (connectionClose conn))

-- | Replaces the record function of the Session with the provided one.
--
-- This is useful for custom recorder actions, or if you need to catch any exceptions
-- thrown by the IO action and don't wish them to bubble up to the statistics.
withRecordFunction :: (forall a. Recorder -> String -> IO a -> IO a) -> WSConnection -> WSConnection
withRecordFunction r sess = sess {cRecord = r}

sendAndReceiveMessage :: (ToJSON a, FromJSON b) => (a -> b -> Bool) -> WSConnection -> String -> a -> IO b
sendAndReceiveMessage p = withRecorder1 $ \ conn _ reqPayload -> do
                                          let waitForResponse = do
                                                msg <- WS.receiveData conn
                                                case decode msg of
                                                  -- TODO view patterns
                                                  Just b -> if p reqPayload b then return b else waitForResponse
                                                  Nothing -> waitForResponse
                                          WS.sendTextData conn (encode reqPayload)
                                          waitForResponse

sendAndReceiveMessageWithId :: (ToJSON a, WithMessageId a, FromJSON b, WithMessageId b) => WSConnection -> String -> a -> IO b
sendAndReceiveMessageWithId = sendAndReceiveMessage (\ a b -> messageId a == messageId b)

-- fireAndForgetMessage :: ToJSON a => Connection -> String -> a -> IO ()
-- fireAndForgetMessage = withRecorder _wat2

-- this records things. It's not ideal, but an more acurate
-- implementation is harder. Pull requests welcome.
-- withRecorder :: (WS.Connection -> String -> IO a) -> WSConnection -> String -> IO a
-- withRecorder f (WSConnection {..}) key = cRecord cRecorder key $ f cConnection key

withRecorder1 :: (WS.Connection -> String -> a -> IO b) -> WSConnection -> String -> a -> IO b
withRecorder1 f (WSConnection {..}) key b = cRecord cRecorder key $ f cConnection key b
