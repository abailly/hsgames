module Main

import Data.Strings
import System
import System.Info
import Network.Socket
import Network.Socket.Data
import Network.Socket.Raw

runServer : Port -> IO ()
runServer port = do
  Right sock <- socket AF_INET Stream 0
        | Left fail => putStrLn $ "Failed to open socket: " ++ show fail
  res <- bind sock (Just (Hostname "localhost")) port
  if res /= 0
    then putStrLn $ "Failed to bind socket with error: " ++ show res
    else do
      res <- listen sock
      if res /= 0
        then putStrLn $ "Failed to listen on socket with error: " ++ show res
        else runAccept port sock

  where
    serve : Port -> Socket -> IO ()
    serve port sock = do
      Right  (str, _) <- recv sock 1024
        | Left err => putStrLn ("Failed to receive on socket with error: " ++ show err)
      putStrLn ("Received: " ++ str)
      Right n <- send sock ("echo: " ++ str)
        | Left err => putStrLn ("Server failed to send data with error: " ++ show err)
      pure ()

    runAccept : Port -> Socket -> IO ()
    runAccept port sock = do
          Right (s, _) <- accept sock
            | Left err => putStrLn ("Failed to accept on socket with error: " ++ show err)
          forked <- fork (serve port s)
          putStrLn $ "forked thread "
          runAccept port sock

runClient : Int -> Port -> IO ()
runClient sec serverPort = do
  Right sock <- socket AF_INET Stream 0
    | Left fail => putStrLn ("Failed to open socket: " ++ show fail)
  res <- connect sock (Hostname "localhost") serverPort
  if res /= 0
    then putStrLn ("Failed to connect client to port " ++ show serverPort ++ ": " ++ show res)
    else do
      putStrLn $ "connected to server at " ++ show serverPort ++ ", sleeping for " ++ show sec
      sleep sec
      Right n <- send sock ("hello world!")
        | Left err => putStrLn ("Client failed to send data with error: " ++ show err)
      Right (str, _) <- recv sock 1024
        | Left err => putStrLn ("Client failed to receive on socket with error: " ++ show err)
      -- assuming that stdout buffers get flushed in between system calls, this is "guaranteed"
      -- to be printed after the server prints its own message
      putStrLn ("Received: " ++ str)

partial
main : IO ()
main = do
  (_ :: args) <- getArgs
  case args of
    ("server" :: _) => runServer 56789
    ("client" :: count :: _) =>
      case parseInteger count of
        Nothing => putStrLn $ "cannot parse " ++ count ++ " as an integer number"
        (Just x) => runClient x 56789
