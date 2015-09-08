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

runServer :: PortNumber -> IO ()
runServer port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 5
  server <- Server <$> newTVarIO M.empty
  forever $ do
    (clientSock, _) <- accept sock
    h <- socketToHandle clientSock ReadWriteMode
    hSetBuffering h NoBuffering
    forkIO $ runReaderT (interpretCommands h) server

interpretCommands :: Handle -> ReaderT Server IO ()
interpretCommands handle = do
  res <- interpretClientCommand handle
  case res of
   Nothing -> return ()
   Just s  -> liftIO (hPrint handle s) >> interpretCommands handle

interpretClientCommand :: Handle -> ReaderT Server IO (Maybe String)
interpretClientCommand handle = do
  line <- liftIO $ readClientCommand handle
  case line of
   Left _ -> return Nothing
   Right dat ->  handleCommand handle (read dat)
  where
    readClientCommand :: Handle -> IO (Either IOError String)
    readClientCommand = try . hGetLine

data Server = Server { activeGames :: TVar (M.Map GameId ActiveGame) }

data ActiveGame = ActiveGame { gameId           :: GameId
                             , numberOfHumans   :: Int
                             , numberOfRobots   :: Int
                             , registeredHumans :: Connections
                             , gameThread       :: Maybe ThreadId
                             }

handleCommand :: Handle -> Command -> ReaderT Server IO (Maybe String)
handleCommand _ (NewGame numHumans numRobots) = startNewGame numHumans numRobots
handleCommand h (JoinGame player game)        = joinGame h player game

startNewGame :: Int -> Int -> ReaderT Server IO (Maybe String)
startNewGame numh numr = do
  Server{..} <- ask
  newId <- liftIO randomGameId
  let newGame = ActiveGame newId numh numr M.empty Nothing
  liftIO $ atomically $ modifyTVar' activeGames  (M.insert newId newGame)
  return $ Just newId

randomGameId :: IO GameId
randomGameId = getStdGen >>= return . take 8 . randomRs ('A','Z')

joinGame :: Handle -> PlayerName -> GameId -> ReaderT Server IO (Maybe String)
joinGame h player game = do
  Server{..} <- ask
  res <- liftIO $ atomically $ addPlayerToActiveGame h player game activeGames
  case res of
   Left msg -> return $ Just msg
   Right g ->
     if M.size (registeredHumans g) == numberOfHumans g
     then runFilledGame g >> return Nothing
     else return $ Just $ "player " ++ player ++ " registered for game "++ game

addPlayerToActiveGame :: Handle -> PlayerName -> GameId -> TVar (M.Map GameId ActiveGame) -> STM (Either String ActiveGame)
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

runFilledGame :: ActiveGame -> ReaderT Server IO String
runFilledGame ActiveGame{..} = do
  tid <- liftIO $ forkIO (runGameServer gameId numberOfRobots registeredHumans)
  s <- ask
  liftIO $ atomically $ modifyTVar' (activeGames s) (M.adjust (\ g -> g { gameThread = Just tid}) gameId)
  return $ "game " ++ gameId

runGameServer :: GameId -> Int -> Connections -> IO ()
runGameServer gid numRobots clients  = do
  g <- getStdGen
  let connections = M.insert "Console" (Cnx stdin stdout) clients
  let robots = map ((,Robot) . ("robot " ++) . show) [ 1 .. numRobots ]
  runReaderT (runPromptM playerInputHandler $ initialisedGame gid g (map (\ (p,_) -> (p,Human)) (M.toList clients) ++ robots) >>= interpretCommand) connections

