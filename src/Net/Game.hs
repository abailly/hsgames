{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Net.Game where

import           Net.Types
import           Network.Socket
import           Pretty
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
  print $ "Sending " ++ show command
  hPrint h command
  res <- hGetLine h
  putStrLn $ "starting new game " ++ res
  hClose h

listGames :: String -> PortNumber -> IO ()
listGames host port = do
  h <- connectTo host port
  hPrint h ListGames
  res :: [ GameDescription ] <- read `fmap` hGetLine h
  putDoc $ pretty res
  putStrLn ""
  hClose h

