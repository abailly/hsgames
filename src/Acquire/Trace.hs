module Acquire.Trace(trace) where

import           Control.Monad.Trans
import           Data.Time.Clock
import           Data.Time.Format

trace :: (MonadIO m) => String -> m ()
trace msg = liftIO $ do
  t <- getCurrentTime
  putStrLn $ "[" ++ formatTime defaultTimeLocale "%F %T.%q %z" t ++ "] " ++ msg

