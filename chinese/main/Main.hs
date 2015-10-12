{-# LANGUAGE BangPatterns #-}
import           Chinese.Interpreter
import           Control.Monad.Prompt
import           Control.Monad.Reader
import           Data.Map             as M
import           Data.Time.Clock
import           System.IO
import           System.Random

-- For testing purpose only
main :: IO ()
main = do
  g <- newStdGen
  d <- readDictionary "data/hsk2.zh"
  dt <- getCurrentTime
  let game = newGame g "arnaud" d dt
  void $ runReaderT (runPromptM playerInputHandler (runGame game)) (M.fromList [ ("arnaud" , Cnx stdin stdout)])
