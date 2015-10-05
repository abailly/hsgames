{-# LANGUAGE BangPatterns #-}
import           Chinese.Interpreter
import           Control.Monad.Prompt
import           Control.Monad.Reader
import           Data.Map             as M
import           System.IO
import           System.Random

-- For testing purpose only
main :: IO ()
main = do
  g <- newStdGen
  d <- readDictionary "data/hsk2.zh"
  let game = newGame g "arnaud" d
  void $ runReaderT (runPromptM playerInputHandler (interpretCommand game)) (M.fromList [ ("arnaud" , Cnx stdin stdout)])
