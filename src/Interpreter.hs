{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter where

import           Control.Exception    hiding (Handler)
import           Control.Monad
import           Control.Monad.Prompt
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Game
import           Player
import           Robot
import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Random

data Connection = Cnx { hIn  :: Handle
                      , hOut :: Handle
                      }

type Connections = M.Map PlayerName Connection

data PlayerInput a where
  GetOrder    :: Player -> Game -> PlayerInput Order
  PlayedOrder :: Player -> Game -> Order -> PlayerInput ()
  Quit        :: Game -> PlayerInput ()
  SaveGame    :: Game -> PlayerInput ()
  LoadGame    :: PlayerInput (Maybe Game)

type Handler m a = PlayerInput a -> m a

data Message = GameState Player GameBoard [Order]
             | Played PlayerName GameBoard Order
             | GameEnds Game
             deriving (Eq, Show, Read)

playerInputHandler :: Handler (ReaderT Connections IO) a
playerInputHandler (GetOrder p@(Player name Human _ _ _) g) = do
  Cnx hin hout <- (M.! name) <$> ask
  liftIO $ playHuman p g hin hout
playerInputHandler (GetOrder p@(Player _ Robot _ _ _) g) = do
  liftIO $ playRobot p g
playerInputHandler (SaveGame g) = liftIO $ writeFile ".acquire.bak" (show g)
playerInputHandler (PlayedOrder p g o) = broadcast (\ n (Cnx _ hout) -> when (n == "Console" ||
                                                                              n /= playerName p &&
                                                                              playerType ((players g) M.! n) /= Robot)
                                                                        (liftIO $ (hPutStrLn hout $ show $ Played (playerName p) (gameBoard g) o) >> hFlush hout))
playerInputHandler LoadGame = do
  e <- liftIO $ doesFileExist ".acquire.bak"
  if e
  then liftIO (readFile ".acquire.bak")  >>= return . Just . read
  else return Nothing
playerInputHandler (Quit game) = do
  broadcast (\ n (Cnx _ hout) -> when (n == "Console" ||
                                       playerType ((players game) M.! n) /= Robot)
                                 (liftIO $ (hPutStrLn hout $ show $ GameEnds game) >> hFlush hout))
  liftIO exitSuccess

broadcast :: (Monad m) => (PlayerName -> Connection -> m ()) -> ReaderT Connections m ()
broadcast f = ask >>= mapM_ (lift . uncurry f) . M.assocs

playHuman :: Player -> Game -> Handle -> Handle -> IO Order
playHuman p@Player{..} game hin hout = do let plays = (possiblePlay game)
                                          hPutStrLn hout $ show $ GameState p (gameBoard game) plays
                                          hFlush hout
                                          r <- tryJust (guard . isEOFError) $ hGetLine hin
                                          case r of
                                           Left  _    -> return Cancel
                                           Right line -> return $ plays !! (read line - 1)

initialisedGame :: StdGen -> [(PlayerName,PlayerType)] -> Prompt PlayerInput Game
initialisedGame g num = do
  loaded <- prompt $ LoadGame
  case loaded of
   Nothing -> return $ newGame g num
   Just  game -> return game

interpretCommand :: Game -> Prompt PlayerInput ()
interpretCommand game@Game{..} = do
  prompt $ SaveGame game
  let player = currentPlayer game
  order <- prompt $ GetOrder player game
  prompt $ PlayedOrder player game order
  if   order == Cancel
  then prompt (Quit game)
  else interpretCommand $ play game order
