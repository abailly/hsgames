{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Prompt
import           Game
import           Pretty
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Random

data PlayerInput a where
  DisplayGame :: Game -> PlayerInput ()
  GetOrder :: Game -> PlayerInput Order
  Quit :: PlayerInput ()

main :: IO ()
main = do
  [numTiles] <- getArgs
  g <- getStdGen
  let game = newGame g (read numTiles)
  putStrLn "Initial state:"

  runPromptM handlePlayerInput $ interpretCommand game

handlePlayerInput :: PlayerInput a -> IO a
handlePlayerInput GetOrder = do r <- tryJust (guard . isEOFError) $ getLine
                          case r of
                           Left  e    -> return Cancel
                           Right line -> return $ read line
handlePlayerInput Quit     = exitSuccess
handlePlayerInput (DisplayGame game) = do putDoc $ pretty game
                                    putStrLn ""

interpretCommand :: Game -> Prompt PlayerInput ()
interpretCommand game@Game{..} = do
  prompt $ DisplayGame game
  order <- prompt $ GetOrder game
  if   order == Cancel
  then prompt Quit
  else interpretCommand $ play game order
