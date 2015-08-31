{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Net.Player where

import           Control.Monad.Reader
import           Interpreter
import           Network.Socket
import           Player
import           Pretty
import           System.IO


waitForPlayers :: Socket -> Int -> [(PlayerName, Socket)] -> IO [(PlayerName, Socket)]
waitForPlayers _    0 clients = return clients
waitForPlayers sock n clients = do
  (clientSock, clientAddr) <- accept sock
  putStrLn $ "client connection from " ++ show clientAddr
  (name, _, _) <- recvFrom clientSock 64
  putStrLn $ "connecting player " ++ show name
  waitForPlayers sock (n-1) ((name,clientSock):clients)

runPlayer :: String -> PortNumber -> PlayerName -> IO ()
runPlayer host port player = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints { addrFamily = AF_INET, addrSocketType = Stream }
  server:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  connect sock  (addrAddress server)
  _ <- send sock player
  putStrLn $ "registering " ++ player ++ " with server at address " ++ show (addrAddress server)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  play player h

play :: PlayerName -> Handle -> IO ()
play player handle = do
  dat <- hGetLine handle
  m <- handleMessage (read dat)
  case m of
   Just response -> void $ hPutStrLn handle response
   Nothing       -> return ()
  play player handle

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
