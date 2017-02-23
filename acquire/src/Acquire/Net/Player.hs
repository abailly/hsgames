{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Acquire.Net.Player where

import           Acquire.Game
import           Acquire.Net.Types
import           Acquire.Pretty
import           Acquire.Trace
import           Control.Monad.Reader
import           Network.Socket
import           System.IO

-- | High-level encapsulation of I/O exchanges with player
data InOut = InOut { input        :: IO String
                   , output       :: Message -> IO ()
                   , outputResult :: Result -> IO ()
                   }

consoleIO :: InOut
consoleIO = InOut getLine printMessage printResult
  where
    printResult :: Result -> IO ()
    printResult res = putStrLn (render $ pretty res) >> putStrLn ""

    printMessage :: Message -> IO ()
    printMessage (GameState player board plays) = do
      let board' = highlightPlayableTiles board plays
      putStrLn (render $ pretty board')
      putStrLn ""
      putStrLn (render $ pretty player)
      putStrLn ""
      putStrLn $ "Your move, " ++ playerName player ++ " ?"
      mapM_ (\ (p,n :: Int) -> putStrLn $ show n ++ "- " ++ show p) (zip plays [1 .. ])
    printMessage (Played player _ order) = do
      putStrLn $ "player "++ player ++ " played " ++ show order
    printMessage (GameEnds game)  = do
      putStrLn "game ends"
      putStrLn (render $ pretty game)
      putStrLn ""

runPlayer :: String -> PortNumber -> PlayerName -> GameId
          -> InOut
          -> IO ()
runPlayer host port player game io = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints { addrFamily = AF_INET, addrSocketType = Stream }
  server:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  connect sock  (addrAddress server)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  hPutStrLn h (show $ JoinGame player game)
  trace $ "registering " ++ player ++ " with server at address " ++ show (addrAddress server)
  readResult h player io

readResult :: Handle -> PlayerName -> InOut -> IO ()
readResult h player io@InOut{..} = do
  ln <- hGetLine h
  let res :: Result = read  ln
  outputResult res
  case res of
   GameStarts _ -> hFlush h >> askForPlay player h io
   _            -> readResult h player io

askForPlay :: PlayerName -> Handle -> InOut -> IO ()
askForPlay player handle io = do
  dat <- hGetLine handle
  m <- handleMessage (read dat) io
  case m of
   Just response -> void $ hPutStrLn handle response
   Nothing       -> return ()
  askForPlay player handle io

handleMessage :: Message -> InOut -> IO (Maybe String)
handleMessage g@(GameState _ _ _) InOut{..} = do
  output g
  Just `fmap` input
handleMessage m (InOut _ output _) = do
  output m
  return Nothing
