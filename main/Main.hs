module Main where

import           Control.Monad.Prompt
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Interpreter
import           System.Environment
import           System.IO
import           System.Random

main :: IO ()
main = do
  [numTiles] <- getArgs
  let num = read numTiles
  g <- getStdGen
  let connections = M.fromList [ ("arnaud", Cnx stdin stdout)
                               , ("bernard", Cnx stdin stdout)
                               ]
  runReaderT (runPromptM playerInputHandler $ initialisedGame g num >>= interpretCommand) connections
