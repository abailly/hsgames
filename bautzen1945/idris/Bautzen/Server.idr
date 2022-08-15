||| Game server for Bautzen 1945
module Bautzen.Server

import Bautzen.Options

import Bautzen.Games
import Bautzen.Id
import Bautzen.Network
import Bautzen.REPL
import Bautzen.Store

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

record GameHandler where
  constructor MkGameHandler
  cin : Channel String
  cout : Channel String
  game : ThreadID

data MessageResult : Type where
  MsgError : (err : String) -> MessageResult
  MsgQuery : (result : String) -> MessageResult
  MsgEvent : (event : String) -> (gameId : Id) -> MessageResult

record GamesHandler where
  constructor MkGamesHandler
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

mkGamesHandler : Store store => store -> IO GamesHandler
mkGamesHandler s = do
  games <- newIORef initialGames
  mutex <- makeMutex
  pure $ MkGamesHandler (handle games mutex)
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
           (Right (GamesResQuery r)) =>
             MsgQuery (show $ cast { to = JSON } r)
           (Right (GamesResError x)) =>
             MsgError $ show $ cast { to = JSON } x

handleClient : Logger -> Socket -> SocketAddress -> Id -> GameHandler -> IO (ThreadID, ThreadID)
handleClient log socket addr clientId hdlr =
 (,) <$> fork handleInput <*> fork handleOutput
 where
   client : String
   client =  show clientId

   stopHandler : GameHandler -> IO ()
   stopHandler hdlr = do
     channelPut hdlr.cin "STOP"

   stopOutput : GameHandler -> IO ()
   stopOutput hdlr =
     channelPut hdlr.cout "STOP"

   handleInput : IO ()
   handleInput = do
      Right msg <- receive log socket
        | Left err => do log ("[" ++ client ++ "] closing input handler: " ++ err) ; close socket ; stopHandler hdlr ; stopOutput hdlr
      log $ "[" ++ client ++ "] received '" ++ msg ++ "'"
      channelPut hdlr.cin msg
      handleInput

   handleOutput : IO ()
   handleOutput = do
     res <- channelGet hdlr.cout
     case res of
        "STOP" => do
           log $ "[" ++ client ++ "] closing output handler"
        _ => do
            log $ "[" ++ client ++ "] sending result: '" ++ res ++ "'"
            Right l <- send socket (toWire res)
              | Left err => do log ("[" ++ client ++ "] failed to send message " ++ show err) ; close socket ; stopHandler hdlr
            log $ "[" ++ client ++ "] sent result"
            handleOutput

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


||| Input channel where commands/queries are read from as `String`
||| Output channel where results are sent to as `String`
||| Map of clients to their `GameHandler`s
||| This is needed to ensure proper cleanup of handlers when the
||| `commandLoop` stops.
||| Map from games `Id` to a map from client `Id` to their output channels
||| This is used to dispatch `GamesResEvent` results to all parties which are
||| connected on a given game.
||| The client `Id` this `commandLoop` is related to
||| The message handler for the game this loop is related to.
commandLoop : Logger ->
              Channel String ->
              Channel String ->
              IORef (SortedMap Id GameHandler) ->
              IORef (SortedMap Id (SortedMap Id (Channel String))) ->
              Id ->
              GamesHandler ->
              IO ()
commandLoop log cin cout clients gamesOutput clientId gamesState = do
   msg <- channelGet cin
   case msg of
     -- Poison pill
     "STOP" => do
       log $ "[" ++ client ++ "] Stopping command loop"
       modifyIORef gamesOutput $ map removeOutput
       modifyIORef clients (delete clientId)
     _ => do
       res <- gamesState.handleMessage clientId msg
       handleOutputsForResult res
       commandLoop log cin cout clients gamesOutput clientId gamesState
  where
   removeOutput :  SortedMap Id (Channel String) ->
                   SortedMap Id (Channel String)
   removeOutput = delete clientId

   client : String
   client = show clientId

   handleOutputsForResult : MessageResult -> IO ()
   handleOutputsForResult (MsgError err) = channelPut cout err
   handleOutputsForResult (MsgQuery q) = channelPut cout q
   handleOutputsForResult (MsgEvent event gameId) = do
      modifyIORef gamesOutput $ \ gmap => case lookup gameId gmap of
        Nothing => insert gameId (insert clientId cout empty) gmap
        Just outs =>
          case lookup clientId outs of
            Nothing => insert gameId (insert clientId cout outs) gmap
            Just _ => gmap
      Just outs <- lookup gameId <$> readIORef gamesOutput
        | Nothing => pure () -- TODO: should never happen? prove it...
      log $ "[" ++ client ++ "] dispatching result: " ++ event
      traverse_ (\out => channelPut out event) outs

||| Main server `accept`ing loop.
serve : Logger ->
        Socket ->
        IORef (SortedMap Id GameHandler) ->
        IORef (SortedMap Id (SortedMap Id (Channel String))) ->
        GamesHandler ->
        IO (Either String ())
serve log sock clients gamesOutput gamesState = do
  log "awaiting clients"
  Right (s, addr) <- accept sock
    | Left err => pure (Left $ "Failed to accept on socket with error: " ++ show err)
  log $ "client connecting from " ++ show addr
  Right clientId <- clientHandshake log s
    |  Left err => do log ("failed to identify client: " ++ show err) ; close s; serve log sock clients gamesOutput gamesState
  cs <- readIORef clients
  hdlr <- maybe (mkChannelsFor clientId) pure (lookup clientId cs)
  _ <- handleClient log s addr clientId hdlr
  log $ "[" ++ show clientId ++ "] forked client handler"
  serve log sock clients gamesOutput gamesState
 where

   mkChannelsFor : Id -> IO GameHandler
   mkChannelsFor clientId = do
      cin <- makeChannel
      cout <- makeChannel
      loopPid <- fork $ do
        log $ "[" ++ show clientId ++ "] forked command loop"
        commandLoop log cin cout clients gamesOutput clientId gamesState
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
          gamesState <- mkGamesHandler =<< makeFileStore "games.store"
          serve (mkLogger verbosity) sock hdlrs gamesOutput gamesState
