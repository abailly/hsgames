-- | A robot player
module Robot where

import           Game
import           Player
import           System.Random

playRobot :: Player -> Game -> IO Order
playRobot _ game = do let plays = possiblePlay game
                      p <- randomRIO (0,length plays - 1)
                      return $ plays !! p

