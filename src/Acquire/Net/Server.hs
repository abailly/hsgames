{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Acquire.Net.Server(runServer, PortNumber) where

import qualified Acquire.Game             as G
import           Acquire.Game.Core        (players)
import           Acquire.Interpreter
import           Acquire.Net.Types
import           Acquire.Trace
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception        (try)
import           Control.Monad.Prompt
import           Control.Monad.Reader
import           Data.List
import qualified Data.Map                 as M
import           Network.Socket
import           System.Directory
import           System.IO
import           System.Random

type Server = TVar (M.Map GameId ActiveGame)

runServer :: PortNumber -> IO ()
runServer port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 5
  existingGames <- readSavedGames
  server <- newTVarIO existingGames
  void $ async (garbageCollector server)
  forever $ do
    (clientSock, _) <- accept sock
    h <- socketToHandle clientSock ReadWriteMode
    hSetBuffering h NoBuffering
    forkIO $ runReaderT (interpretCommands h) server

readSavedGames :: IO (M.Map GameId ActiveGame)
readSavedGames = do
  saved <- filter isSaveFile <$> getDirectoryContents "."
  M.fromList <$> mapM loadSaved saved

  where
    isSaveFile f = ".acquire" `isPrefixOf` f && ".bak" `isSuffixOf` f
    loadSaved f = do
      g <- read <$> readFile f
      let gid = G.gameId g
          nh = length $ filter isHuman (M.elems $ players g)
          nr = length $ filter isRobot (M.elems $ players g)
      return $ (gid, ActiveGame gid nh nr M.empty [] Nothing)


-- | Periodically checks existing games to see if they are still running
-- If a game is found to be stopped, its state is cleaned:
--
--  * all player's threads are sent a @ThreadKilled@ signal
--  * game thread is set to nothing
garbageCollector :: Server -> IO ()
garbageCollector server = forever $ do
  tids <- liftIO $ atomically $ do
    gamesMap <- readTVar server
    cleanedGames <- mapM cleanupStoppedGames (M.elems gamesMap)
    writeTVar server (M.fromList $ map ((\ g -> (gameId g, g)) . x1) cleanedGames)
    return cleanedGames
  forM_ tids doCleanupGame
  threadDelay $ 10 * 1000 * 1000
    where

      x1 (a,_,_) = a

      doCleanupGame :: (ActiveGame, [ThreadId], [Connection]) -> IO ()
      doCleanupGame (_, [], _) = return ()
      doCleanupGame (g, tids, cnxs) = do
        trace $ "cleaning up game : " ++ gameId g
        trace $ "closing connections : " ++ show cnxs
        mapM_ closeConnection cnxs
        trace $ "stopping threads : " ++ show tids
        mapM_ killThread tids

      cleanupStoppedGames :: ActiveGame -> STM (ActiveGame, [ThreadId], [Connection])
      cleanupStoppedGames g@ActiveGame{..} =
        case gameThread of
         Nothing -> return (g,[],[])
         Just as -> do
           res <- pollSTM as
           maybe (return (g,[],[])) (const $ return (g { gameThread = Nothing, connectionThreads = [], registeredHumans = M.empty }, connectionThreads, M.elems registeredHumans)) res


interpretCommands :: Handle -> ReaderT Server IO ()
interpretCommands handle = do
  res <- interpretClientCommand handle
  case res of
   Nothing -> trace ("terminating commands loop for "  ++ show handle)
   Just s  -> liftIO (trace ("sending " ++ show s) >> hPutStrLn handle (show s)) >> interpretCommands handle

interpretClientCommand :: Handle -> ReaderT Server IO (Maybe Result)
interpretClientCommand handle = do
  ln <- liftIO $ readClientCommand handle
  trace ("received command from: " ++ show handle ++ ", " ++ show ln)
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
  let newGame = ActiveGame newId numh numr M.empty [] Nothing
  liftIO $ atomically $ modifyTVar' activeGames  (M.insert newId newGame)
  return $ Just $ NewGameStarted newId

randomGameId :: IO GameId
randomGameId = newStdGen >>= return . take 8 . randomRs ('A','Z')

joinGame :: Handle -> PlayerName -> GameId -> ReaderT Server IO (Maybe Result)
joinGame h player game = do
  activeGames <- ask
  tid <- liftIO $ myThreadId
  res <- liftIO $ atomically $ addPlayerToActiveGame h tid player game activeGames
  either (return . Just . ErrorMessage)
    startGameIfAllHumansRegistered
    res
   where
     startGameIfAllHumansRegistered g = if M.size (registeredHumans g) == numberOfHumans g
                                        then runFilledGame g
                                        else return $ Just $ PlayerRegistered player game

addPlayerToActiveGame :: Handle -> ThreadId -> PlayerName -> GameId -> Server -> STM (Either String ActiveGame)
addPlayerToActiveGame h tid player game activeGames = do
  games <- readTVar activeGames
  case M.lookup game games of
   Nothing               -> return $ Left $ "no active game "++ game
   Just g@ActiveGame{..} -> case gameThread of
                             Just _  -> return $ Left $ "game "++ game ++ " already started"
                             Nothing -> do
                               let players = M.insert player (Cnx h h) registeredHumans
                                   g'      = g { registeredHumans = players, connectionThreads = tid : connectionThreads }
                               modifyTVar' activeGames (M.insert gameId g')
                               return $ Right g'

runFilledGame :: ActiveGame -> ReaderT Server IO (Maybe Result)
runFilledGame ActiveGame{..} = do
  liftIO $ notifyStartup gameId registeredHumans connectionThreads
  asyncGame <- liftIO $ async (runGameServer gameId numberOfRobots registeredHumans)
  trace ("started game " ++ gameId)
  activeGames <- ask
  liftIO $ atomically $ modifyTVar' activeGames (M.adjust (\ g -> g { gameThread = Just asyncGame}) gameId)
  return Nothing

runGameServer :: GameId -> Int -> Connections -> IO Game
runGameServer gid numRobots clients  = do
  g <- getStdGen
  let connections = M.insert "Console" (Cnx stdin stdout) clients
      robots      = map ((,Robot) . ("robot " ++) . show) [ 1 .. numRobots ]
  forM_ (M.elems connections) (\ (Cnx _ hout) -> hFlush hout)
  runReaderT (runPromptM playerInputHandler $ initialisedGame gid g (map (\ (p,_) -> (p,Human)) (M.toList clients) ++ robots) >>= interpretCommand) connections

notifyStartup :: GameId -> Connections -> [ ThreadId ]  -> IO ()
notifyStartup gid cnx threads = do
  mytid <- myThreadId
  forM_ (M.elems cnx) (\ (Cnx _ hout) -> hPutStrLn hout $ show (GameStarts gid))
  forM_ threads (\ tid -> when (tid /= mytid) $  -- we don't kill the thread we are running in...
                          trace ("killing thread " ++ show tid ++ " for game " ++ gid) >> killThread tid)
