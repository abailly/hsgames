||| Command-line client for Bautzen 1945
module Bautzen.Client

import Bautzen.Options

import Bautzen.Network

import Data.Strings.Extra

import Builtin
import Prelude
import Data.Nat
import Data.List
import Debug.Trace
import Network.Socket
import Network.Socket.Data
import Network.Socket.Raw
import System
import System.File

sendCommand : Socket -> SExp -> IO (Either String String)
sendCommand socket command = do
  let cmd = toWire command
  Right l <- send socket cmd
    | Left err => pure $ Left ("failed to send message " ++ show err)
  Right (str, _) <- recv socket 6
    | Left err => pure $ Left ("failed to receive length of message " ++ show err)
  case parseInteger (unpack str) 0 of
    Nothing => pure $ Left ("fail to parse '" ++ str ++ "' to expected number of characters, ignoring")
    Just len => do Right (msg, _) <- recv socket (fromInteger len)
                     | Left err => pure $ Left ("failed to read message " ++ show err)
                   pure (Right msg)

export
openConnection : (host:String) -> (port: Int) -> IO (Either String Socket)
openConnection host port = do
  Right sock <- socket AF_INET Stream 0
        | Left fail => pure (Left $ "Failed to open socket: " ++ show fail)
  res <- connect sock (Hostname host) port
  if res /= 0
    then pure (Left $ "Failed to connect socket with error: " ++ show res)
    else pure (Right sock)

runREPL : Socket -> IO ()
runREPL sock =
  do putStr "> "
     eof <- fEOF stdin
     if eof
       then pure ()
       else do x <- getLine
               case parseSExp x of
                    Right out =>
                        do Right result <- sendCommand sock out
                                 | Left err => do putStrLn ("Error sending command: " ++ err) ; runREPL sock
                           putStrLn result
                           runREPL sock
                    Left err => do putStrLn ("Invalid command: " ++ err) ; runREPL sock

export
client : Options -> IO (Either String ())
client (MkOptions port host _)  = do
  Right cnx <- openConnection host port
        | Left err => pure (Left err)
  runREPL cnx
  pure (Right ())
