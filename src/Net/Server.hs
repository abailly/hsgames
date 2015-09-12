{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Net.Server(runServer, PortNumber) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception      (try)
import           Control.Monad.Prompt
import           Control.Monad.Reader
import qualified Data.Map               as M
import           Interpreter
import           Net.Types
import           Network.Socket
import           System.IO
import           System.Random

type Server = TVar (M.Map GameId ActiveGame)

runServer :: PortNumber -> IO ()
runServer port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 5
  server <- newTVarIO M.empty
  forever $ do
    (clientSock, _) <- accept sock
    h <- socketToHandle clientSock ReadWriteMode
    hSetBuffering h NoBuffering
    forkIO $ runReaderT (interpretCommands h) server

interpretCommands :: Handle -> ReaderT Server IO ()
interpretCommands handle = do
  res <- interpretClientCommand handle
  liftIO $ putStrLn $ "result of client command from " ++ show handle ++ " is " ++ show res
  case res of
   Nothing -> return ()
   Just s  -> liftIO (hPutStrLn handle $ show s) >> interpretCommands handle

interpretClientCommand :: Handle -> ReaderT Server IO (Maybe Result)
interpretClientCommand handle = do
  ln <- liftIO $ readClientCommand handle
  liftIO $ putStrLn $ "received command: " ++ show ln
  either (const $ return Nothing) (handleCommand handle . read) ln
  where
    readClientCommand :: Handle -> IO (Either IOError String)
    readClientCommand = try . hGetLine

handleCommand :: Handle -> Command -> ReaderT Server IO (Maybe Result)
handleCommand _ (NewGame numHumans numRobots) = startNewGame numHumans numRobots
handleCommand h (JoinGame player game)        = joinGame h player game
handleCommand _ (StartingGame _)              = return Nothing
handleCommand _ ListGames                     = do
  activeGames <- ask
  games <- liftIO $ atomically $ readTVar activeGames
  return $ Just $ GamesList $ map gamesList (M.elems games)

startNewGame :: Int -> Int -> ReaderT Server IO (Maybe Result)
startNewGame numh numr = do
  activeGames <- ask
  newId <- liftIO randomGameId
  let newGame = ActiveGame newId numh numr M.empty Nothing
  liftIO $ atomically $ modifyTVar' activeGames  (M.insert newId newGame)
  return $ Just $ NewGameStarted newId

randomGameId :: IO GameId
randomGameId = newStdGen >>= return . take 8 . randomRs ('A','Z')

joinGame :: Handle -> PlayerName -> GameId -> ReaderT Server IO (Maybe Result)
joinGame h player game = do
  activeGames <- ask
  res <- liftIO $ atomically $ addPlayerToActiveGame h player game activeGames
  either (return . Just . ErrorMessage)
    startGameIfAllHumansRegistered
    res
   where
     startGameIfAllHumansRegistered g = if M.size (registeredHumans g) == numberOfHumans g
                                        then runFilledGame g
                                        else return $ Just $ PlayerRegistered player game

addPlayerToActiveGame :: Handle -> PlayerName -> GameId -> Server -> STM (Either String ActiveGame)
addPlayerToActiveGame h player game activeGames = do
  games <- readTVar activeGames
  case M.lookup game games of
   Nothing               -> return $ Left $ "no active game "++ game
   Just g@ActiveGame{..} -> case gameThread of
                             Just _  -> return $ Left $ "game "++ game ++ " already started"
                             Nothing -> do
                               let players = M.insert player (Cnx h h) registeredHumans
                                   g'      = g { registeredHumans = players }
                               modifyTVar' activeGames (M.insert gameId g')
                               return $ Right g'

runFilledGame :: ActiveGame -> ReaderT Server IO (Maybe Result)
runFilledGame ActiveGame{..} = do
  tid <- liftIO $ forkIO (runGameServer gameId numberOfRobots registeredHumans)
  activeGames <- ask
  liftIO $ atomically $ modifyTVar' activeGames (M.adjust (\ g -> g { gameThread = Just tid}) gameId)
  return Nothing

runGameServer :: GameId -> Int -> Connections -> IO ()
runGameServer gid numRobots clients  = do
  g <- getStdGen
  let connections = M.insert "Console" (Cnx stdin stdout) clients
      robots      = map ((,Robot) . ("robot " ++) . show) [ 1 .. numRobots ]
  notifyStartup gid connections
  runReaderT (runPromptM playerInputHandler $ initialisedGame gid g (map (\ (p,_) -> (p,Human)) (M.toList clients) ++ robots) >>= interpretCommand) connections

notifyStartup :: GameId -> Connections -> IO ()
notifyStartup gid cnx = forM_ (M.elems cnx) (\ (Cnx _ hout) -> hPutStrLn hout $ show (GameStarts gid))
