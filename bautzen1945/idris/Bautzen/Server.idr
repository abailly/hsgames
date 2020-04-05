||| Game server for Bautzen 1945
module Bautzen.Server

import Bautzen.Options

import Bautzen.Game
import Bautzen.REPL

import Data.List.Extra
import Data.Strings.Extra

import Builtin
import Prelude
import Data.Nat
import Data.List
import Network.Socket
import Network.Socket.Data
import Network.Socket.Raw
import System


padWith0 : Int -> String
padWith0 k =
  let num = show k
      len = Prelude.length num
  in case isLTE len 6 of
       Yes _ =>  let padding = Prelude.pack $ Data.List.Extra.replicate (6 - len) '0'
                 in padding ++ num
       No _ => num

handleClient : Socket -> SocketAddress -> Game -> IO ()
handleClient socket addr game = do
  Right (str, _) <- recv socket 6 -- receive 6 characters representing the length of message to read
    | Left err => do putStrLn ("failed to receive length of message " ++ show err) ; close socket -- TODO error handling
  putStrLn ("received  " ++ str)
  case parseInteger (unpack str) 0 of
    Nothing => do putStrLn ("Fail to parse " ++ str ++ "to a number") ; close socket -- TODO should raise an error somewhere...
    Just len => do Right (msg, _) <- recv socket (fromInteger len)
                     | Left err => do putStrLn ("failed to read message " ++ show err) ; close socket
                   putStrLn $ "received  " ++ msg
                   let (res, game') = commandHandler game msg
                   let lens = padWith0 (prim__strLength res)
                   putStrLn $ "sending  " ++ show res
                   send socket (lens ++ res)
                   handleClient socket addr game'


serve : Socket -> IO (Either String ())
serve sock = do
  Right (s, addr) <- accept sock
    | Left err => pure (Left $ "Failed to accept on socket with error: " ++ show err)
  pid <- fork (handleClient s addr initialGame)
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
