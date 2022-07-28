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

mkLogger : Verbosity -> Logger
mkLogger Quiet _ = pure ()
mkLogger (Verbose name) msg =
  putStrLn  $ "[" ++ name ++ "] " ++ msg

receiveMessage : Logger -> Socket -> IO (Either String String)
receiveMessage log socket = do
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

handleClient : Logger -> Socket -> SocketAddress -> GameHandler -> IO ()
handleClient log socket addr hdlr = do
  Right msg <- receiveMessage log socket
    | Left err => do log err ; close socket
  log $ "received '" ++ msg ++ "'"
  channelPut hdlr.cin msg
  res <- channelGet hdlr.cout
  log $ "result is '" ++ show res ++ "'"
  let lens = padWith0 (cast $ length res)
  log $ "sending " ++ lens ++ " chars"
  Right l <- send socket (lens ++ res)
    | Left err => do log ("failed to send message " ++ show err) ; close socket -- TODO error handling
  log $ "sent result"
  handleClient log socket addr hdlr


clientHandshake : Logger -> Socket -> IO (Either String Id)
clientHandshake log sock = do
   Right msg <- join . map makeId <$> receiveMessage log sock
     | Left err => pure (Left err)
   Right _ <- send sock (toWire "OK")
     | Left err => pure (Left $ show err)
   pure $ Right msg

commandLoop : Channel String -> Channel String -> Id -> Games -> IO ()
commandLoop cin cout clientId game = do
   msg <- channelGet cin
   case msg of
     -- Poison pill
     "STOP" => pure ()
     _ => do
       let (res, game') = handleCommand clientId game msg
       channelPut cout res
       commandLoop cin cout clientId game'

serve : Logger -> Socket -> IORef (SortedMap Id GameHandler) ->  IO (Either String ())
serve log sock clients  = do
  log "awaiting clients"
  Right (s, addr) <- accept sock
    | Left err => pure (Left $ "Failed to accept on socket with error: " ++ show err)
  log $ "client connecting from " ++ show addr
  Right clientId <- clientHandshake log s
    |  Left err => do log ("failed to identify client, " ++ show err) ; close s; serve log sock clients
  cs <- readIORef clients
  hdlr <- maybe (mkChannelsFor clientId) pure (lookup clientId cs)
  pid <- fork $ do
    handleClient log s addr hdlr
    stopHandler hdlr
    modifyIORef clients (delete clientId)
  log $ "forked client handler for " ++ show @{AsString} clientId
  serve log sock clients
 where
   mkChannelsFor : Id -> IO GameHandler
   mkChannelsFor clientId = do
      cin <- makeChannel
      cout <- makeChannel
      loopPid <- fork $ commandLoop cin cout clientId initialGames
      let hdlr = MkGameHandler cin cout loopPid
      modifyIORef clients (insert clientId hdlr)
      pure hdlr

   stopHandler : GameHandler -> IO ()
   stopHandler hdlr = channelPut hdlr.cin "STOP"

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
          serve (mkLogger verbosity) sock hdlrs
