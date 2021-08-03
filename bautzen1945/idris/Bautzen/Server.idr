||| Game server for Bautzen 1945
module Bautzen.Server

import Bautzen.Options

import Bautzen.Game
import Bautzen.Network
import Bautzen.REPL

import Data.Strings.Extra

import Builtin
import Prelude
import Data.Nat
import Data.List
import Network.Socket
import Network.Socket.Data
import Network.Socket.Raw
import System

Logger : Type
Logger = String -> IO ()

mkLogger : Verbosity -> Logger
mkLogger Quiet _ = pure ()
mkLogger (Verbose name) msg =
  putStrLn  $ "[" ++ name ++ "] " ++ msg


handleClient : Logger -> Socket -> SocketAddress -> Game -> IO ()
handleClient log socket addr game = do
  log "waiting for input"
  Right (str, _) <- recv socket 6 -- receive 6 characters representing the length of message to read
    | Left err => do log ("failed to receive length of message " ++ show err) ; close socket -- TODO error handling
  log $ "received  " ++ str
  case parseInteger (unpack str) 0 of
    Nothing => do log ("fail to parse '" ++ str ++ "' to expected number of characters, ignoring") ; handleClient log socket addr game
    Just len => do Right (msg, _) <- recv socket (fromInteger len)
                     | Left err => do log ("failed to read message " ++ show err) ; handleClient log socket addr game
                   log $ "received '" ++ msg ++ "'"
                   let (res, game') = commandHandler game msg
                   log $ "result is '" ++ show res ++ "'"
                   let lens = padWith0 (cast $ length res)
                   log $ "sending " ++ lens ++ " chars"
                   Right l <- send socket (lens ++ res)
                     | Left err => do log ("failed to send message " ++ show err) ; close socket -- TODO error handling
                   log $ "sent result"
                   handleClient log socket addr game'


serve : Logger -> Socket -> IO (Either String ())
serve log sock = do
  log "awaiting clients"
  Right (s, addr) <- accept sock
    | Left err => pure (Left $ "Failed to accept on socket with error: " ++ show err)
  log $ "client connecting from " ++ show addr
  pid <- fork (handleClient log s addr initialGame)
  log $ "forked client handler"
  serve log sock

export
server : Options -> IO (Either String ())
server (MkOptions port host _ verbosity) = do
  Right sock <- socket AF_INET Stream 0
        | Left fail => pure (Left $ "Failed to open socket: " ++ show fail)
  res <- bind sock (Just (Hostname host)) port
  if res /= 0
    then pure (Left $ "Failed to bind socket with error: " ++ show res)
    else do
      res <- listen sock
      if res /= 0
        then pure (Left $ "Failed to listen on socket with error: " ++ show res)
        else serve (mkLogger verbosity) sock
