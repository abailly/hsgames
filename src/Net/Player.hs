{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Net.Player where

import           Control.Monad.Reader
import           Game                 (highlightPlayableTiles)
import           Interpreter
import           Net.Types
import           Network.Socket
import           Player
import           Pretty
import           System.IO


runPlayer :: String -> PortNumber -> PlayerName -> GameId -> IO ()
runPlayer host port player game = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints { addrFamily = AF_INET, addrSocketType = Stream }
  server:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  connect sock  (addrAddress server)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  hPutStrLn h (show $ JoinGame player game)
  putStrLn $ "registering " ++ player ++ " with server at address " ++ show (addrAddress server)
  putStrLn $ "joining " ++ game
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
  let board' = highlightPlayableTiles board plays
  putDoc (pretty board')
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
