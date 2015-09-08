{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Net.Game where

import           Net.Types
import           Network.Socket
import           System.IO


runNewGame :: String -> PortNumber -> Int -> Int -> IO ()
runNewGame host port numHumans numRobots = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints { addrFamily = AF_INET, addrSocketType = Stream }
  server:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  connect sock  (addrAddress server)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  let command = NewGame numHumans numRobots
  print $ "Sending " ++ show command
  hPrint h command
  res <- hGetLine h
  putStrLn $ "starting new game " ++ res
  hClose h
