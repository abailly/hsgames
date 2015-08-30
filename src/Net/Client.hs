{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Net.Client where

import           Control.Monad.Reader
import           Interpreter
import           Network.Socket
import           Player
import           Pretty
import           System.IO


waitForClients :: Socket -> Int -> [(PlayerName, Socket)] -> IO [(PlayerName, Socket)]
waitForClients _    0 clients = return clients
waitForClients sock n clients = do
  (clientSock, clientAddr) <- accept sock
  putStrLn $ "client connection from " ++ show clientAddr
  (playerName, _, _) <- recvFrom clientSock 64
  putStrLn $ "connecting player " ++ show playerName
  waitForClients sock (n-1) ((playerName,clientSock):clients)

runClient :: String -> PortNumber -> PlayerName -> IO ()
runClient host port player = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints { addrFamily = AF_INET, addrSocketType = Stream }
  server:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  connect sock  (addrAddress server)
  _ <- send sock player
  putStrLn $ "registering " ++ player ++ " with server at address " ++ show (addrAddress server)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  playClient player h

playClient :: PlayerName -> Handle -> IO ()
playClient player handle = do
  dat <- hGetLine handle
  m <- handleMessage (read dat)
  case m of
   Just response -> void $ hPutStrLn handle response
   Nothing       -> return ()
  playClient player handle

handleMessage :: Message -> IO (Maybe String)
handleMessage (GameState player board plays) = do
  putDoc (pretty board)
  putStrLn ""
  putDoc (pretty player)
  putStrLn ""
  putStrLn $ "Your move, " ++ Player.playerName player ++ " ?"
  mapM_ (\ (p,n :: Int) -> putStrLn $ show n ++ "- " ++ show p) (zip plays [1 .. ])
  line <- getLine
  return $ Just line
handleMessage (Played player _ order) = do
  putStrLn $ "player "++ player ++ " played " ++ show order
  return Nothing
handleMessage (GameEnds game) = do
  putStrLn "game ends"
  putDoc (pretty game)
  putStrLn ""
  return Nothing
