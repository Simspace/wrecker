{-# LANGUAGE ScopedTypeVariables, TypeOperators, OverloadedStrings
  #-}
{-# LANGUAGE DeriveGeneric, FlexibleInstances, QuasiQuotes #-}
{-# LANGUAGE CPP, FlexibleContexts, UndecidableInstances,
  RecordWildCards #-}
{-# LANGUAGE DeriveFunctor, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}
#ifndef _SERVER_IS_MAIN_
module Server where
#endif
import Control.Concurrent
import Control.Concurrent.NextRef (NextRef)
import qualified Control.Concurrent.NextRef as NextRef
import Control.Exception
import qualified Control.Immortal as Immortal
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Aeson.QQ
import Data.Maybe (listToMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Network.Socket (Socket)
import qualified Network.Socket as N
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp
       (Port, defaultSettings, openFreePort)
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment
import Web.Scotty (ActionM, ScottyM, json)
import qualified Web.Scotty as Scotty
import Wrecker
import Wrecker.Runner
import qualified Wrecker.Statistics as Wrecker

data Envelope a = Envelope
    { value :: a
    } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (Envelope a)

rootRef :: Int -> Text
rootRef port = T.pack $ "http://localhost:" ++ show port

jsonE :: ToJSON a => a -> ActionM ()
jsonE = json . Envelope

data Root a = Root
    { root :: a
    , products :: a
    , cartsIndex :: a
    , cartsIndexItems :: a
    , usersIndex :: a
    , login :: a
    , checkout :: a
    } deriving (Show, Eq, Functor)

type RootInt = Root Int

instance Applicative Root where
    pure x =
        Root
        { root = x
        , products = x
        , login = x
        , usersIndex = x
        , cartsIndex = x
        , cartsIndexItems = x
        , checkout = x
        }
    f <*> x =
        Root
        { root = root f $ root x
        , products = products f $ products x
        , login = login f $ login x
        , usersIndex = usersIndex f $ usersIndex x
        , cartsIndex = cartsIndex f $ cartsIndex x
        , cartsIndexItems = cartsIndexItems f $ cartsIndexItems x
        , checkout = checkout f $ checkout x
        }

app :: RootInt -> Port -> ScottyM ()
app Root {..} port = do
    let host = rootRef port
    Scotty.get "/root" $ do
        liftIO $ threadDelay root
        jsonE
            [aesonQQ|
           { "products" : #{host <> "/products" }
           , "carts"    : #{host <> "/carts"    }
           , "users"    : #{host <> "/users"    }
           , "login"    : #{host <> "/login"    }
           , "checkout" : #{host <> "/checkout" }
           }
          |]
    Scotty.get "/products" $ do
        liftIO $ threadDelay products
        jsonE
            [aesonQQ|
             [ #{host <> "/products/0"}
             ]
           |]
    Scotty.get "/product/:id" $ do
        liftIO $ threadDelay products
        jsonE
            [aesonQQ|
          { "summary" : "shirt" }
          |]
    Scotty.get "/carts" $
    -- sleepDist gen carts
     do
        jsonE
            [aesonQQ|
          [ #{host <> "/carts/0"}
          ]
          |]
    Scotty.get "/carts/:id" $ do
        liftIO $ threadDelay cartsIndex
        jsonE
            [aesonQQ|
          { "items" : #{host <> "/carts/0/items"}
          }
          |]
    Scotty.post "/carts/:id/items" $ do
        liftIO $ threadDelay cartsIndexItems
        jsonE
            [aesonQQ|
          #{host <> "/carts/0/items"}
          |]
    Scotty.get "/users" $
    -- sleepDist gen users
     do
        jsonE
            [aesonQQ|
          [ #{host <> "/users/0"}
          ]
          |]
    Scotty.get "/users/:id" $ do
        liftIO $ threadDelay usersIndex
        jsonE
            [aesonQQ|
          { "cart"     : #{host <> "/carts/0"}
          , "username" : "example"
          }
          |]
    Scotty.post "/login" $ do
        liftIO $ threadDelay login
        jsonE
            [aesonQQ|
          #{host <> "/users/0"}
          |]
    Scotty.post "/checkout" $ do
        liftIO $ threadDelay checkout
        jsonE ()

run :: RootInt -> IO (Port, Immortal.Thread, ThreadId, NextRef AllStats)
run = start Nothing

stop :: (Port, ThreadId, NextRef AllStats) -> IO AllStats
stop (_, threadId, ref) = do
    killThread threadId
    NextRef.readLast ref

toKey :: Wai.Request -> String
toKey x =
    case Wai.pathInfo x of
        ["root"] -> "/root"
        ["products"] -> "/products"
        "carts":_:"items":_ -> "/carts/0/items"
        "carts":_:_ -> "/carts/0"
        "users":_ -> "/users/0"
        ["login"] -> "/login"
        ["checkout"] -> "/checkout"
        _ -> error "FAIL! UNKNOWN REQUEST FOR EXAMPLE!"

recordMiddleware :: Recorder -> Wai.Application -> Wai.Application
recordMiddleware recorder waiApp req sendResponse =
    record recorder (toKey req) $! waiApp req $ \res -> sendResponse res

getASocket :: Maybe Port -> IO (Port, Socket)
getASocket =
    \case
        Just port -> do
            s <- N.socket N.AF_INET N.Stream N.defaultProtocol
            localhost <- N.inet_addr "127.0.0.1"
            N.bind s (N.SockAddrInet (fromIntegral port) localhost)
            N.listen s 1000
            return (port, s)
        Nothing -> openFreePort

start :: Maybe Port -> RootInt -> IO (Port, Immortal.Thread, ThreadId, NextRef AllStats)
start mport dist = do
    (port, socket) <- getASocket mport
    (ref, recorderThread, recorder) <- newStandaloneRecorder
    scottyApp <- Scotty.scottyApp $ app dist port
    threadId <-
        flip forkFinally (\_ -> N.close socket) $
        Warp.runSettingsSocket defaultSettings socket $ recordMiddleware recorder $ scottyApp
    return (port, recorderThread, threadId, ref)

main :: IO ()
main = do
    xs <- getArgs
    let delay = maybe 0 read $ listToMaybe xs
    (port, socket) <- getASocket $ Just 3000
    (ref, recorderThread, recorder) <- newStandaloneRecorder
    scottyApp <- Scotty.scottyApp $ app (pure delay) port
    (Warp.runSettingsSocket defaultSettings socket $ recordMiddleware recorder $ scottyApp) `finally`
        (do N.close socket
            Immortal.stop recorderThread
            allStats <- NextRef.readLast ref
            putStrLn $ Wrecker.pprStats Nothing Path allStats)
