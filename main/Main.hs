{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception    hiding (Handler)
import           Control.Monad
import           Control.Monad.Prompt
import           Game
import           Player
import           Pretty
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Random


data PlayerInput a where
  GetOrder :: Player -> Game -> PlayerInput Order
  Quit     :: PlayerInput ()
  SaveGame :: Game -> PlayerInput ()
  LoadGame :: PlayerInput (Maybe Game)

type Handler a = PlayerInput a -> IO a

playerInputHandler :: Handler a
playerInputHandler (GetOrder Player{..} game) = do putDoc $ pretty game
                                                   putStrLn ""
                                                   putStrLn $ "Your move, " ++ playerName ++ " ?"
                                                   let plays = possiblePlay game
                                                   forM_ (zip plays [1 .. ]) (\ (p,n :: Int) -> putStrLn $ show n ++ "- " ++ show p)
                                                   r <- tryJust (guard . isEOFError) $ getLine
                                                   case r of
                                                    Left  e    -> return Cancel
                                                    Right line -> return $ plays !! (read line - 1)
playerInputHandler (SaveGame g) = writeFile ".acquire.bak" (show g)
playerInputHandler LoadGame = do
  e <- doesFileExist ".acquire.bak"
  if e
  then readFile ".acquire.bak"  >>= return . Just . read
  else return Nothing
playerInputHandler Quit     = exitSuccess


main :: IO ()
main = do
  [numTiles] <- getArgs
  let num = read numTiles
  g <- getStdGen
  runPromptM playerInputHandler $ initialisedGame g num >>= interpretCommand

initialisedGame :: StdGen -> Int -> Prompt PlayerInput Game
initialisedGame g num = do
  loaded <- prompt $ LoadGame
  case loaded of
   Nothing -> return $ newGame g num
   Just  g -> return g

interpretCommand :: Game -> Prompt PlayerInput ()
interpretCommand game@Game{..} = do
  prompt $ SaveGame game
  let player = currentPlayer game
  order <- prompt $ GetOrder player game
  if   order == Cancel
  then prompt Quit
  else interpretCommand $ play game order
