{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import           Control.Monad.Prompt
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Interpreter
import           Player
import           System.Environment
import           System.IO
import           System.Random

main :: IO ()
main = do
  playersDescription <- getArgs
  let players = map read playersDescription
  g <- getStdGen
  let connections = M.fromList $ ("Console", Cnx stdin stdout) : map ((,Cnx stdin stdout) . fst) (filter ((== Human) . snd) players)
  runReaderT (runPromptM playerInputHandler $ initialisedGame g players >>= interpretCommand) connections
