{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Control.Monad.Prompt
import           Game
import           Pretty
import           System.IO

data Input a where
  Play :: Input Order
  Quit :: Input ()

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  let game = newGame
  putStrLn "Initial state:"

  interpretCommand game

interpretCommand :: Game -> IO ()
interpretCommand game@Game{..} = do
  putDoc $ pretty game
  putStrLn ""
--  command <- readLn
--  prompt command






