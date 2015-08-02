{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Prompt
import           Game
import           Pretty
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Random

data Input a where
  DisplayGame :: Game -> Input ()
  GetOrder :: Input Order
  Quit :: Input ()

main :: IO ()
main = do
  g <- getStdGen
  let game = newGame g
  putStrLn "Initial state:"

  runPromptM handleInput $ interpretCommand game

handleInput :: Input a -> IO a
handleInput GetOrder = do r <- tryJust (guard . isEOFError) $ getLine
                          case r of
                           Left  e    -> return Cancel
                           Right line -> return $ read line
handleInput Quit     = exitSuccess
handleInput (DisplayGame game) = do putDoc $ pretty game
                                    putStrLn ""

interpretCommand :: Game -> Prompt Input ()
interpretCommand game@Game{..} = do
  prompt $ DisplayGame game
  order <- prompt GetOrder
  if   order == Cancel
  then prompt Quit
  else interpretCommand $ play game order





