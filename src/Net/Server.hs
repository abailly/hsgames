{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Net.Server(runServer, PortNumber) where

import           Control.Concurrent
import           Control.Monad.Prompt
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map             as M
import           Interpreter
import           Net.Types
import           Network.Socket
import           System.IO
import           System.Random

runServer :: PortNumber -> IO ()
runServer port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 5
  (clientSock, _) <- accept sock
  h <- socketToHandle clientSock ReadWriteMode
  hSetBuffering h NoBuffering
  void $ execStateT (interpretCommands h) (Server M.empty)


interpretCommands :: Handle -> StateT Server IO ()
interpretCommands handle = forever $ do
  dat <- liftIO $ hGetLine handle
  res <- handleCommand handle (read dat)
  liftIO $ hPutStrLn handle res

data Server = Server { activeGames :: M.Map GameId ActiveGame }

data ActiveGame = ActiveGame { gameId           :: GameId
                             , numberOfHumans   :: Int
                             , numberOfRobots   :: Int
                             , registeredHumans :: Connections
                             , gameThread       :: Maybe ThreadId
                             }

handleCommand :: Handle -> Command -> StateT Server IO String
handleCommand _ (NewGame numHumans numRobots) = startNewGame numHumans numRobots
handleCommand h (JoinGame player game)        = joinGame h player game

startNewGame :: Int -> Int -> StateT Server IO String
startNewGame numh numr = do
  s@Server{..} <- get
  newId <- liftIO randomGameId
  let newGame = ActiveGame newId numh numr M.empty Nothing
  put $ s { activeGames = M.insert newId newGame activeGames }
  return newId

randomGameId :: IO GameId
randomGameId = getStdGen >>= return . take 8 . randomRs ('A','Z')

joinGame :: Handle -> PlayerName -> GameId -> StateT Server IO String
joinGame h player game = do
  Server{..} <- get
  case M.lookup game activeGames of
   Nothing               -> return $ "no active game "++ game
   Just g@ActiveGame{..} -> case gameThread of
                             Just _  -> return $ "game "++ game ++ " already started"
                             Nothing -> do
                               let players = M.insert player (Cnx h h) registeredHumans
                                   g' = g { registeredHumans = players }
                               if M.size players == numberOfHumans
                               then runFilledGame g'
                               else return $ "player " ++ player ++ " registered for game "++ game

runFilledGame :: ActiveGame -> StateT Server IO String
runFilledGame ActiveGame{..} = do
  tid <- liftIO $ forkIO (runGameServer numberOfRobots registeredHumans)
  s <- get
  put (s { activeGames = M.adjust (\ g -> g { gameThread = Just tid}) gameId (activeGames s) })
  return $ "starting game " ++ gameId

runGameServer :: Int -> Connections -> IO ()
runGameServer numRobots clients  = do
  g <- getStdGen
  let connections = M.insert "Console" (Cnx stdin stdout) clients
  let robots = map ((,Robot) . ("robot " ++) . show) [ 1 .. numRobots ]
  runReaderT (runPromptM playerInputHandler $ initialisedGame g (map (\ (p,_) -> (p,Human)) (M.toList clients) ++ robots) >>= interpretCommand) connections

