module GameServer.Clients.Player where

import Data.Aeson (encode, decode)
import GameServer.Log
import Data.Maybe
import Control.Monad.Reader
import Network.Socket
import System.IO
import GHC.Natural

import GameServer.Clients.Messages
import GameServer.Utils
import GameServer.Clients.IO

-- | High-level encapsulation of I/O exchanges with player
data InOut m = InOut { input        :: m String
                     , output       :: Result -> m ()
                     }

runNewGame :: String -> PortNumber -> Natural -> Natural -> IO Result
runNewGame host port numHumans numRobots = do
  h <- connectTo host port
  send h (encode $ NewGame numHumans numRobots)
  res :: Result <- (fromJust . decode) `fmap` receive h
  disconnect h
  return res

listGames :: String -> PortNumber -> IO Result
listGames host port = do
  h <- connectTo host port
  send h (encode ListGames)
  res :: Result <- (fromJust . decode) `fmap` receive h
  disconnect h
  return res

runPlayer :: LoggerEnv IO
          -> String -> PortNumber
          -> Id -> Id
          -> InOut IO
          -> IO ()
runPlayer logger host port player game io = do
  h <- connectTo host port
  send h (encode $ JoinGame player game)
  logInfo logger  $ "registering " ++ show player ++ " with server, waiting for GameStarts"
  waitForStart logger h player io
  logInfo logger $ "start playing " <> show player <> "@" <> show game
  playerLoop logger h player io

waitForStart :: LoggerEnv IO -> ServerConnection IO -> Id -> InOut IO -> IO ()
waitForStart logger handle player io@InOut{..} = do
  res :: Result <- (fromJust . decode) `fmap` receive handle
  logInfo logger $ "[waiting] received message " <> show res <> " from server"
  output res
  case res of
   GameStarts _ -> pure ()
   _            -> waitForStart logger handle player io

playerLoop :: LoggerEnv IO -> ServerConnection IO -> Id -> InOut IO -> IO ()
playerLoop logger handle player io = do
  dat <- (fromJust . decode) `fmap` receive handle
  logInfo logger $ "[playing] received message " <> show dat <> " from server"
  m <- handleCommand dat io
  case m of
    Just response -> do
      logInfo logger $ "[playing] sending message " <> show response <> " to server"
      send handle (encode response)
    Nothing       -> return ()
  playerLoop logger handle player io

handleCommand :: Result -> InOut IO -> IO (Maybe String)
handleCommand msg@(InputRequired payload) InOut{..} = do
   output msg
   Just `fmap` input
handleCommand msg InOut{output} = do
  output msg
  pure Nothing
