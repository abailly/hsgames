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
import           Pretty
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
  GetOrder :: Player -> Game -> PlayerInput Order
  Quit     :: PlayerInput ()
  SaveGame :: Game -> PlayerInput ()
  LoadGame :: PlayerInput (Maybe Game)

type Handler m a = PlayerInput a -> m a

playerInputHandler :: Handler (ReaderT Connections IO) a
playerInputHandler (GetOrder p@(Player name Human _ _ _) g) = do
  Cnx hin hout <- (M.! name) <$> ask
  liftIO $ playHuman p g hin hout
playerInputHandler (GetOrder p@(Player _ Robot _ _ _) g) = do
  liftIO $ playRobot p g
playerInputHandler (SaveGame g) = liftIO $ writeFile ".acquire.bak" (show g)
playerInputHandler LoadGame = do
  e <- liftIO $ doesFileExist ".acquire.bak"
  if e
  then liftIO (readFile ".acquire.bak")  >>= return . Just . read
  else return Nothing
playerInputHandler Quit     = liftIO exitSuccess

playHuman :: Player -> Game -> Handle -> Handle -> IO Order
playHuman Player{..} game hin hout = do hPutDoc hout $ pretty game
                                        hPutStrLn hout ""
                                        hPutStrLn hout $ "Your move, " ++ playerName ++ " ?"
                                        let plays = possiblePlay game
                                        forM_ (zip plays [1 .. ]) (\ (p,n :: Int) -> hPutStrLn hout $ show n ++ "- " ++ show p)
                                        r <- tryJust (guard . isEOFError) $ hGetLine hin
                                        case r of
                                         Left  _    -> return Cancel
                                         Right line -> return $ plays !! (read line - 1)

initialisedGame :: StdGen -> Int -> Prompt PlayerInput Game
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
  if   order == Cancel
  then prompt Quit
  else interpretCommand $ play game order
