{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Acquire.Net.Game where

import           Acquire.Net.Types
import           Acquire.Pretty
import           Network.Socket
import           System.IO

connectTo :: String -> PortNumber -> IO Handle
connectTo host port = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints { addrFamily = AF_INET, addrSocketType = Stream }
  server:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  connect sock  (addrAddress server)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  return h

runNewGame :: String -> PortNumber -> Int -> Int -> IO ()
runNewGame host port numHumans numRobots = do
  h <- connectTo host port
  let command = NewGame numHumans numRobots
  hPrint h command
  res :: Result <- read `fmap` hGetLine h
  putDoc $ pretty res
  putStrLn ""
  hClose h

listGames :: String -> PortNumber -> IO ()
listGames host port = do
  h <- connectTo host port
  hPrint h ListGames
  res :: Result <- read `fmap` hGetLine h
  putDoc $ pretty res
  putStrLn ""
  hClose h
