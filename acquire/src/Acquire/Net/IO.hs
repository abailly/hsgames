{-# LANGUAGE OverloadedStrings #-}
module Acquire.Net.IO where

import qualified Data.Aeson as A
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import System.IO
import GHC.Stack(HasCallStack)

send :: (HasCallStack, A.ToJSON a) => Handle -> a -> IO ()
send h a = do
  let bs = A.encode a <> "\n"
  LBS.hPut h bs
  hFlush h

receive :: (HasCallStack, A.FromJSON a) => Handle -> IO (Either String a)
receive h = do
  bs <- LBS.fromStrict <$> BS.hGetLine h
  pure $ bimap (<> show bs) id (A.eitherDecode bs)
