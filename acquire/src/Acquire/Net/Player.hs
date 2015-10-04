{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Acquire.Net.Player where

import           Acquire.Game         (highlightPlayableTiles)
import           Acquire.Interpreter
import           Acquire.Net.Types
import           Acquire.Player       as P
import           Acquire.Pretty
import           Control.Monad.Reader
import           Network.Socket
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
  readResult h player

readResult :: Handle -> PlayerName -> IO ()
readResult h player = do
  ln <- hGetLine h
  let res :: Result = read  ln
  putDoc (pretty res) >> putStrLn ""
  case res of
   GameStarts _ -> hFlush h >> putStrLn "starting play " >> play player h
   _            -> readResult h player

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
  putStrLn $ "Your move, " ++ P.playerName player ++ " ?"
  mapM_ (\ (p,n :: Int) -> putStrLn $ show n ++ "- " ++ show p) (zip plays [1 .. ])
  ln <- getLine
  return $ Just ln
handleMessage (Played player _ order) = do
  putStrLn $ "player "++ player ++ " played " ++ show order
  return Nothing
handleMessage (GameEnds game) = do
  putStrLn "game ends"
  putDoc (pretty game)
  putStrLn ""
  return Nothing
