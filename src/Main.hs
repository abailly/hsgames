module Main where

import           Game
import           Pretty

main :: IO ()
main = do
  let game = newGame
  putDoc $ pretty game

