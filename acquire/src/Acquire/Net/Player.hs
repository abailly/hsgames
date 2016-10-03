{-# LANGUAGE RecordWildCards     #-}
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

-- | High-level encapsulation of I/O exchanges with player
data InOut = InOut { input  :: IO String
                   , output :: String -> IO ()
                   }

consoleIO :: InOut
consoleIO = InOut getLine putStrLn

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
  putStrLn $ "registering " ++ player ++ " with server at address " ++ show (addrAddress server)
  readResult h player io

readResult :: Handle -> PlayerName -> InOut -> IO ()
readResult h player io@InOut{..} = do
  ln <- hGetLine h
  let res :: Result = read  ln
  output (render $ pretty res) >> output ""
  case res of
   GameStarts _ -> hFlush h >> output "starting play " >> play player h io
   _            -> readResult h player io

play :: PlayerName -> Handle -> InOut -> IO ()
play player handle io = do
  dat <- hGetLine handle
  m <- handleMessage (read dat) io
  case m of
   Just response -> void $ hPutStrLn handle response
   Nothing       -> return ()
  play player handle io

handleMessage :: Message -> InOut -> IO (Maybe String)
handleMessage (GameState player board plays) InOut{..} = do
  let board' = highlightPlayableTiles board plays
  output (render $ pretty board')
  output ""
  output (render $ pretty player)
  output ""
  output $ "Your move, " ++ P.playerName player ++ " ?"
  mapM_ (\ (p,n :: Int) -> output $ show n ++ "- " ++ show p) (zip plays [1 .. ])
  ln <- input
  return $ Just ln
handleMessage (Played player _ order) (InOut _ output) = do
  output $ "player "++ player ++ " played " ++ show order
  return Nothing
handleMessage (GameEnds game) (InOut _ output) = do
  output "game ends"
  output (render $ pretty game)
  output ""
  return Nothing
