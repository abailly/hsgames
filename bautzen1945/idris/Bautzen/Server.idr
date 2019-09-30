||| Game server for Bautzen 1945
module Bautzen.Server

import Bautzen.Options

import Bautzen.Game
import Bautzen.REPL

import Data.String.Extra


import Network.Socket
import Network.Socket.Data
import Network.Socket.Raw
import System
import System.Concurrency.Channels

padWith0 : Nat -> String
padWith0 k =
  let num = show k
  in case isLTE (length num) 6 of
       Yes _ =>  let padding = (pack $ replicate (6 - length num) '0')
                 in padding ++ num
       No _ => num

handleClient : Socket -> SocketAddress -> Game -> IO ()
handleClient socket addr game = do
  Right (str, _) <- recv socket 6 -- receive 6 characters representing the length of message to read
    | Left err => do putStrLn ("failed to receive length of message " ++ show err) ; close socket -- TODO error handling
  case parseInteger (unpack str) 0 of
    Nothing => do putStrLn ("Fail to parse " ++ str ++ "to a number") ; close socket -- TODO should raise an error somewhere...
    Just len => do Right (msg, _) <- recv socket (fromInteger len)
                     | Left err => do putStrLn ("failed to read message " ++ show err) ; close socket
                   let (res, game') = commandHandler game msg
                   let lens = padWith0 (Strings.length res)
                   send socket (lens ++ res)
                   handleClient socket addr game'


serve : Socket -> IO (Either String ())
serve sock = do
  Right (s, addr) <- accept sock
    | Left err => pure (Left $ "Failed to accept on socket with error: " ++ show err)
  Just pid <- spawn (handleClient s addr initialGame)
    | Nothing => do close s
                    pure (Left "Failed to spawn process to handle client, giving up ")
  serve sock

export
server : Options -> IO (Either String ())
server (MkOptions port host) = do
  Right sock <- socket AF_INET Stream 0
        | Left fail => pure (Left $ "Failed to open socket: " ++ show fail)
  res <- bind sock (Just (Hostname host)) port
  if res /= 0
    then pure (Left $ "Failed to bind socket with error: " ++ show res)
    else do
      res <- listen sock
      if res /= 0
        then pure (Left $ "Failed to listen on socket with error: " ++ show res)
        else serve sock
