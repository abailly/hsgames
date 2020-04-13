{-| Handles interaction with game players' clients -}
module GameServer.Clients where

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
import Data.Text(pack, unpack)
import GHC.Generics
import Network.HTTP.Types.Status
import Network.Socket
import Network.WebSockets
       ( Connection
       , rejectRequest
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

import GameServer.Types
import GameServer.Game
import GameServer.Log
import GameServer.Utils
import GameServer.Clients.Player
import GameServer.Clients.Messages
import Debug.Trace

data ClientConnection = ClientConnection
    { inChan           :: InChan String
    , outChan          :: OutChan String
    , clientConnection :: Connection
    , serverPump       :: Maybe (Async ())
    , clientPump       :: Maybe (Async ())
    }

findBackend ::
  [ GameBackend ] -> SBS.ByteString -> Maybe (String, PortNumber)
findBackend backends url = do
  let parts = SBS.split (toEnum $ fromEnum '/') url
  backend <- selectBackend backends =<< case parts of
    ("": "games": "Acquire" :_) -> Just Acquire
    ("": "games": "Bautzen1945" :_) -> Just Bautzen1945
    _ -> Nothing
  pure (gameHost backend, fromIntegral $ gamePort backend)


handleWS ::
  LoggerEnv IO ->
  TVar (M.Map SBS.ByteString (IORef ClientConnection)) ->
  [ GameBackend ] ->
  PendingConnection ->
  IO ()
handleWS logger cnxs backends pending = do
  let key = requestPath $ pendingRequest pending
      backend = findBackend backends key
  logInfo logger $ "got websocket request with key: " ++ show key
  case backend of
    Just (host, port) -> do
      connection <- acceptRequest pending
      ref <- getOrMakeChannels key connection
      withPingThread connection 30 (pure ()) $ do
        logInfo logger $ "starting client for backend at (" ++ host ++ "," ++ show port ++ ")"
        handleClient logger ref host port connection
    Nothing -> do
      logError logger $ "no backend found for URL " ++ show key
      rejectRequest pending "Invalid backend in URL"

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
            logInfo logger  $ "registering new channels with key " ++ show key
            return ref
          Just r -> do
            modifyIORef r (\ c -> c { clientConnection = connection })
            logInfo logger  $ "reusing old channels with key " ++ show key
            return r

handleClient :: LoggerEnv IO -> IORef ClientConnection -> String -> PortNumber -> Connection ->  IO ()
handleClient logger channels host port connection =
  let clientLoop = do
        Text message _ <- receiveDataMessage connection
        logInfo logger  $ "received message: " ++ show message
        case eitherDecode message of
          Left e  -> sendTextData connection (encode $ CommandError $ pack e)
          Right c -> handleCommand c
        clientLoop

  in clientLoop `catch` (\ (e :: ConnectionException) -> do
                            logInfo logger  $ "client error: " ++ (show e) ++", closing everything"
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

    startPlayer :: Id -> Id -> IO ()
    startPlayer playerName gameId = do
      (w,r)   <- newChan
      (w',r') <- newChan

      logInfo logger  $ "start player threads for " ++ show playerName ++ " @" ++ show gameId
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
        logInfo logger  $ "starting game loop for player " ++ show playerName ++ " @" ++ show gameId
        runPlayer logger host port playerName gameId (io (w,r'))
        logInfo logger  $ "stopping game loop for player " ++ show playerName ++ " @" ++ show gameId

      toClient <- async $ do
        logInfo logger  $ "starting response sender for player " ++ show playerName ++ " @" ++ show gameId
        forever $ do
          v <- readChan r
          logInfo logger  $ "sending response to player " ++ show v
          cnx <- clientConnection <$> readIORef channels
          sendTextData cnx v
            `catch` (\ (e :: ConnectionException) -> logInfo logger  $ "response sender error: " ++ (show e))

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

    handleCommand ListGames = do
      r <- listGames host port
      sendTextData connection (encode r)
      logInfo logger ("listing games" :: String)
    handleCommand (NewGame numHumans numRobots) = do
      r <- runNewGame host port numHumans numRobots
      sendTextData connection (encode r)
      logInfo logger  $ "created new game " ++ show r
    handleCommand (JoinGame playerName gameId) = do
      startPlayer playerName gameId
      logInfo logger  $ "player joined game " ++ show gameId
    handleCommand (Action n) = do
      w <- inChan <$> readIORef channels
      writeChan w (unpack n)
      logInfo logger  $ "action " ++ show n
    handleCommand Bye = sendClose connection ("Bye" :: Text)
