{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Acquire.Net (InOut(..), listGames, runNewGame, runPlayer, runServer)
import Acquire.Trace
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.Chan.Unagi (InChan, OutChan, newChan, readChan, writeChan)
import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Monad (forever)
import Data.Aeson
import qualified Data.ByteString as SBS
import Data.Functor
import Data.IORef
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import GHC.Generics
import Messages
import Network.HTTP.Types.Status
import Network.Socket
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets as WaiWS
import Network.Wai.Middleware.Static
import Network.WebSockets
       ( Connection
       , ConnectionException
       , DataMessage(..)
       , PendingConnection
       , RequestHead(..)
       , acceptRequest
       , defaultConnectionOptions
       , pendingRequest
       , receiveDataMessage
       , sendClose
       , sendTextData
       , withPingThread
       )
import System.Environment

newtype CommandError = CommandError { reason :: String }
                     deriving (Generic)

instance ToJSON CommandError

data ClientConnection = ClientConnection
    { inChan           :: InChan String
    , outChan          :: OutChan String
    , clientConnection :: Connection
    , serverPump       :: Maybe (Async ())
    , clientPump       :: Maybe (Async ())
    }

handleWS :: TVar (M.Map SBS.ByteString (IORef ClientConnection)) -> Socket -> PendingConnection -> IO ()
handleWS cnxs serverSocket pending = do
  p <- socketPort serverSocket
  let key = requestPath $ pendingRequest pending
  trace $ "got websocket request with key: " ++ show key
  connection <- acceptRequest pending
  ref <- getOrMakeChannels key connection
  withPingThread connection 30 (pure ()) $
    handleClient ref p connection
  where
    -- This code is unfortunately rather complicated because
    -- we are storing connections and channels mapping client WS connections, based
    -- on path requested. The idea is that the client is supposed to generate random
    -- strings upon first connection so that channels can later be reused when game
    -- is in play, even if it is disconnected. This happens due to timeouts on both
    -- client side and server-side apparently.
    getOrMakeChannels key connection = do
        maybeRef <- atomically $ M.lookup key <$> readTVar cnxs
        case maybeRef of
          Nothing -> do
            (i,o) <- newChan
            let cc = ClientConnection i o connection Nothing Nothing
            ref   <- newIORef cc
            atomically $ modifyTVar cnxs (M.insert key ref)
            trace $ "registering new channels with key " ++ show key
            return ref
          Just r -> do
            modifyIORef r (\ c -> c { clientConnection = connection })
            trace $ "reusing old channels with key " ++ show key
            return r


handleClient :: IORef ClientConnection -> PortNumber -> Connection ->  IO ()
handleClient channels p connection =
  let clientLoop = do
        Text message _ <- receiveDataMessage connection
        trace $ "received message: " ++ show message
        case eitherDecode message of
          Left e  -> sendTextData connection (encode $ CommandError e)
          Right c -> handleCommand c
        clientLoop

  in clientLoop `catch` (\ (e :: ConnectionException) -> do
                            trace $ "client error: " ++ (show e) ++", closing everything"
                            cleanup)

  where
    -- I/O manager for WS connections
    -- We use a pair of `Chan` to read from and write to, encoding
    -- to JSON on the output
    io (w,r') = InOut (input r') (output w) (outputResult w)
      where
        input             = readChan  -- should probably use decode to be symetric, but we send raw strings...
        output       chan = writeChan chan . encode
        outputResult chan = writeChan chan . encode

    startGame sPort playerName gameId = do
      (w,r)   <- newChan
      (w',r') <- newChan

      -- we run 2 asyncs, one for handling player commands and general game play,
      -- the other to pump server's response to WS connection. This seems necessary because
      -- we have 2 connections to handle:
      --
      --  * WS Connection between remote client's UI and this server code,
      --  * Chan-based connection between player's proxy and main server
      --
      -- There should be a way to greatly simplify this code using directly pure version of the game
      -- instead of wrapping the CLI server.
      toServer <- async $ do
        trace $ "starting game loop for player " ++ playerName ++ " @" ++ gameId
        runPlayer "localhost" sPort playerName gameId (io (w,r'))
        trace $ "stopping game loop for player " ++ playerName ++ " @" ++ gameId

      toClient <- async $ do
        trace $ "starting response sender for player " ++ playerName ++ " @" ++ gameId
        forever $ do
          v <- readChan r
          cnx <- clientConnection <$> readIORef channels
          sendTextData cnx v
            `catch` (\ (e :: ConnectionException) -> trace $ "response sender error: " ++ (show e))

      -- we set the write channel to the other end of the pipe used by player loop for
      -- reading. This channel will be used by subsequent commands sent by client and
      -- "pumped" to server
      modifyIORef channels ( \ c -> c { inChan = w'
                                      , serverPump = Just toServer
                                      , clientPump = Just toClient
                                      })

    cleanup = do
      ClientConnection _ _ cnx sp cp <- readIORef channels
      sendClose cnx ("Bye" :: Text)
      maybe (return ()) cancel sp
      maybe (return ()) cancel cp
      modifyIORef channels ( \ c -> c { serverPump = Nothing, clientPump = Nothing })

    handleCommand List = do
      r <- listGames "localhost" p
      sendTextData connection (encode r)
    handleCommand (NewGame numHumans numRobots) = do
      r <- runNewGame "localhost" p numHumans numRobots
      sendTextData connection (encode r)
    handleCommand (JoinGame playerName gameId) =
      startGame p playerName gameId
    handleCommand (Action n) = do
      w <- inChan <$> readIORef channels
      writeChan w (show n)
      trace $ "action " ++ show n
    handleCommand Bye = sendClose connection ("Bye" :: Text)

main :: IO ()
main = do
  [port] <- getArgs
  (s,_) <- runServer 0
  socketPort s >>= trace . (\ p -> "started server on port " ++ show p)
  cnxs <- newTVarIO M.empty
  void $ run (read port) (WaiWS.websocketsOr defaultConnectionOptions (handleWS cnxs s) serveUI)
    where
      serveUI :: Application
      serveUI = staticPolicy (noDots >-> addBase "ui") $ defaultResponse

      defaultResponse :: Application
      defaultResponse _ respond = respond $ responseLBS status404 [] ""
