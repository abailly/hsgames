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
                     , output       :: Message -> m ()
                     , outputResult :: Result -> m ()
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
  send h (encode List)
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
  logInfo logger  $ "registering " ++ show player ++ " with server"
  waitForStart h player io
  playerLoop h player io

waitForStart :: ServerConnection IO -> Id -> InOut IO -> IO ()
waitForStart handle player io@InOut{..} = do
  res :: Result <- (fromJust . decode) `fmap` receive handle
  outputResult res
  case res of
   GameStarts _ -> pure ()
   _            -> waitForStart handle player io

playerLoop :: ServerConnection IO -> Id -> InOut IO -> IO ()
playerLoop handle player io = do
  dat <- (fromJust . decode) `fmap` receive handle
  m <- handleCommand dat io
  case m of
   Just response -> send handle (encode response)
   Nothing       -> return ()
  playerLoop handle player io

handleCommand :: Message -> InOut IO -> IO (Maybe String)
-- handleCommand msg@(GameState _ _ _) InOut{..} = do
--   output msg
--   Just `fmap` input
handleCommand msg (InOut _ output _) = do
  output msg
  return Nothing
