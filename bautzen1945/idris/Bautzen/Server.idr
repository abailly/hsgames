||| Game server for Bautzen 1945
module Bautzen.Server

import Bautzen.Options

import Bautzen.Games
import Bautzen.Id
import Bautzen.Network
import Bautzen.REPL

import Data.Strings.Extra

import Builtin
import Prelude
import Data.Nat
import Data.IORef
import Data.List
import Data.SortedMap
import Language.JSON
import Network.Socket
import Network.Socket.Data
import Network.Socket.Raw
import System
import System.Concurrency

Logger : Type
Logger = String -> IO ()

record GameHandler where
  constructor MkGameHandler
  cin : Channel String
  cout : Channel String
  game : ThreadID

data MessageResult : Type where
  MsgError : (err : String) -> MessageResult
  MsgEvent : (event : String) -> (gameId : Id) -> MessageResult


record GamesState where
  constructor MkGamesState
  ||| Handle a single serialised command from given client @Id@.
  ||| This handler should be threadsafe and returns the result of executing the command, alongside
  ||| the `Id` of the game this command is related to, if it's not a `GamesResultErr`.
  handleMessage : Id -> String -> IO MessageResult

atomically : Mutex -> IO a -> IO a
atomically mutex act = do
  mutexAcquire mutex
  res <- act
  mutexRelease mutex
  pure res

mkGamesState : IO GamesState
mkGamesState = do
  games <- newIORef initialGames
  mutex <- makeMutex
  pure $ MkGamesState (handle games mutex)
  where
    handleCommand' : Id -> (games : Games) -> String -> (Either String (GamesResult games), Games)
    handleCommand' clientId games input =
    case parseCommand games input of
      Left err => (Left err, games)
      Right act =>
        let (res, g) = interpret clientId act games
        in (Right res, g)

    handle : IORef Games -> Mutex -> Id -> String -> IO MessageResult
    handle refGames mutex clientId msg =
      atomically mutex $ do
         g <- readIORef refGames
         let (res, g') = handleCommand' clientId g msg
         writeIORef refGames g'
         pure $ case res of
           (Left err) =>
             MsgError err
           (Right (GamesResEvent event)) =>
             MsgEvent (show $ cast { to = JSON } event) event.id
           (Right (GamesResError x)) =>
             MsgError $ show $ cast { to = JSON } x

mkLogger : Verbosity -> Logger
mkLogger Quiet _ = pure ()
mkLogger (Verbose name) msg =
  putStrLn  $ "[" ++ name ++ "] " ++ msg

receive : Logger -> Socket -> IO (Either String String)
receive log socket = do
  log "waiting for input"
  Right (str, _) <- recv socket 6 -- receive 6 characters representing the length of message to read
    | Left err => pure $ Left ("failed to receive length of message " ++ show err)
  log $ "received  " ++ str
  case parseInteger (unpack str) 0 of
    Nothing => pure $ Left ("fail to parse '" ++ str ++ "' to expected number of characters, ignoring")
    Just len => do
      Right msg <- map fst <$> recv socket (fromInteger len)
        | Left err => pure $ Left (show err)
      pure $ Right msg

handleClient : Logger -> Socket -> SocketAddress -> GameHandler -> IO (ThreadID, ThreadID)
handleClient log socket addr hdlr =
 (,) <$> fork handleInput <*> fork handleOutput
 where

   stopHandler : GameHandler -> IO ()
   stopHandler hdlr = channelPut hdlr.cin "STOP"

   handleInput : IO ()
   handleInput = do
      Right msg <- receive log socket
        | Left err => do log err ; close socket ; stopHandler hdlr
      log $ "received '" ++ msg ++ "'"
      channelPut hdlr.cin msg
      handleInput

   handleOutput : IO ()
   handleOutput = do
     res <- channelGet hdlr.cout
     log $ "dispatchting result: '" ++ show res ++ "'"
     Right l <- send socket (toWire res)
       | Left err => do log ("failed to send message " ++ show err) ; close socket ; stopHandler hdlr
     log $ "sent result"
     handleOutput

data Handshake =
   Connect Id
   | Connected

Cast Handshake JSON where
  cast (Connect k) = JObject [( "tag", JString "Connect"), ("playerKey", cast k)]
  cast Connected = JObject [( "tag", JString "Connected")]

||| Clients are expected to send their `Id` upon connection.
clientHandshake : Logger -> Socket -> IO (Either String Id)
clientHandshake log sock = do
   Right msg <- receive log sock
     | Left err => pure (Left err)
   case parse msg of
     Just (JObject [("tag", JString "Connect"), ("playerKey", JString k)]) =>
       case makeId k of
          Right id => do
            Right _ <- send sock (toWire $ show $ cast {to = JSON } Connected)
              | Left err => pure (Left $ show err)
            pure $ Right id
          Left err => pure $ Left err
     other => pure $ Left $ "invalid handshake " ++ show other


commandLoop : Channel String -> Channel String -> IORef (SortedMap Id GameHandler) -> IORef (SortedMap Id (SortedMap Id (Channel String))) -> Id -> GamesState -> IO ()
commandLoop cin cout clients gamesOutput clientId gamesState = do
   msg <- channelGet cin
   case msg of
     -- Poison pill
     "STOP" =>
       modifyIORef clients (delete clientId)
     _ => do
       res <- gamesState.handleMessage clientId msg
       handleOutputsForResult res
       --traverse_ (\ out => channelPut out res) outs
       commandLoop cin cout clients gamesOutput clientId gamesState
  where
   handleOutputsForResult : MessageResult -> IO ()
   handleOutputsForResult (MsgError err) = channelPut cout err
   handleOutputsForResult (MsgEvent event gameId) = do
      modifyIORef gamesOutput $ \ gmap => case lookup gameId gmap of
        Nothing => insert gameId (insert clientId cout empty) gmap
        Just outs =>
          case lookup clientId outs of
            Nothing => insert gameId (insert clientId cout outs) gmap
            Just _ => gmap
      Just outs <- lookup gameId <$> readIORef gamesOutput
        | Nothing => pure () -- TODO: should never happen? prove it...
      let output = show $ cast {to = JSON} event
      traverse_ (\out => channelPut out output) outs

serve : Logger -> Socket -> IORef (SortedMap Id GameHandler) -> IORef (SortedMap Id (SortedMap Id (Channel String))) -> GamesState ->  IO (Either String ())
serve log sock clients gamesOutput gamesState = do
  log "awaiting clients"
  Right (s, addr) <- accept sock
    | Left err => pure (Left $ "Failed to accept on socket with error: " ++ show err)
  log $ "client connecting from " ++ show addr
  Right clientId <- clientHandshake log s
    |  Left err => do log ("failed to identify client, " ++ show err) ; close s; serve log sock clients gamesOutput gamesState
  cs <- readIORef clients
  hdlr <- maybe (mkChannelsFor clientId) pure (lookup clientId cs)
  _ <- handleClient log s addr hdlr
  log $ "forked client handler for " ++ show @{AsString} clientId
  serve log sock clients gamesOutput gamesState
 where
   mkChannelsFor : Id -> IO GameHandler
   mkChannelsFor clientId = do
      cin <- makeChannel
      cout <- makeChannel
      loopPid <- fork $ commandLoop cin cout clients gamesOutput clientId gamesState
      let hdlr = MkGameHandler cin cout loopPid
      modifyIORef clients (insert clientId hdlr)
      pure hdlr

export
server : Options -> IO (Either String ())
server (MkOptions port host _ verbosity _) = do
  Right sock <- socket AF_INET Stream 0
        | Left fail => pure (Left $ "Failed to open socket: " ++ show fail)
  res <- bind sock (Just (Hostname host)) port
  if res /= 0
    then pure (Left $ "Failed to bind socket with error: " ++ show res)
    else do
      res <- listen sock
      if res /= 0
        then pure (Left $ "Failed to listen on socket with error: " ++ show res)
        else do
          hdlrs <- newIORef empty
          gamesOutput <- newIORef empty
          gamesState <- mkGamesState
          serve (mkLogger verbosity) sock hdlrs gamesOutput gamesState
