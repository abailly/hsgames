module Acquire.Net.IO where

import qualified Data.Aeson as A
import Data.Bifunctor (bimap)
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import System.IO
import GHC.Stack(HasCallStack)

send :: (HasCallStack, A.ToJSON a) => Handle -> a -> IO ()
send h a = do
  let bs = A.encode a
      len = LBS.length bs
      prefix = runPut $ putInt64be len
  LBS.hPut h prefix
  LBS.hPut h bs
  hFlush h

receive :: (HasCallStack, A.FromJSON a) => Handle -> IO (Either String a)
receive h = do
  len <- runGet getInt64be <$> LBS.hGet h 8
  bs <- LBS.hGet h (fromIntegral len)
  pure $ bimap (<> show bs) id (A.eitherDecode bs)
