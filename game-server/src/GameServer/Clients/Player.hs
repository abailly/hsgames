module GameServer.Clients.Player where

import Control.Monad.Reader
import Data.Aeson (decode, eitherDecode, encode)
import Data.Maybe
import GHC.Natural
import GameServer.Clients.IO
import GameServer.Clients.Messages
import GameServer.Log
import GameServer.Utils
import Network.Socket hiding (close)
import System.IO

-- | High-level encapsulation of I/O exchanges with player
data InOut m = InOut
    { input :: m String
    , output :: Result -> m ()
    }

runNewGame :: String -> PortNumber -> Natural -> Natural -> IO Result
runNewGame host port numHumans numRobots = do
    h <- connectTo host port
    send h (encode $ NewGame numHumans numRobots)
    res :: Result <- (fromJust . decode) `fmap` receive h
    close h
    return res

listGames :: LoggerEnv IO -> String -> PortNumber -> IO (Either String Result)
listGames logger host port = do
    h <- connectTo host port
    send h (encode ListGames)
    res :: Either String Result <- eitherDecode `fmap` receive h
    logInfo logger $ "[listGames] received message " <> either id show res
    close h
    return res

runPlayer ::
    LoggerEnv IO ->
    String ->
    PortNumber ->
    Id ->
    Id ->
    InOut IO ->
    IO ()
runPlayer logger host port game player io = do
    h <- connectTo host port
    send h (encode $ JoinGame player game)
    logInfo logger $ "registering " ++ show player ++ " with server, waiting for GameStarts"
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
        _ -> waitForStart logger handle player io

playerLoop :: LoggerEnv IO -> ServerConnection IO -> Id -> InOut IO -> IO ()
playerLoop logger handle player io = do
    dat <- (fromJust . decode) `fmap` receive handle
    logInfo logger $ "[playing] received message " <> show dat <> " from server"
    m <- handleCommand dat io
    case m of
        Just response -> do
            logInfo logger $ "[playing] sending message " <> show response <> " to server"
            send handle (encode response)
        Nothing -> return ()
    playerLoop logger handle player io

handleCommand :: Result -> InOut IO -> IO (Maybe String)
handleCommand msg@(InputRequired payload) InOut{..} = do
    output msg
    Just `fmap` input
handleCommand msg InOut{output} = do
    output msg
    pure Nothing
