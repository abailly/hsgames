{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chinese.Dictionary where

import           Control.Exception
import           Data.Array
import           Prelude           hiding (Word)
import           System.IO
import           System.Random

-- | A dictionary entry for a chinese word
data Word = Word { chinese :: String
                 , pinyin  :: String
                 , french  :: [String]
                 }
            deriving Show

type Dictionary = Array Int Word

readDictionary :: FilePath -> IO Dictionary
readDictionary fname = withFile fname ReadMode $ \ h -> do
  hSetBuffering h NoBuffering
  lns <- readLines h
  return $ dictionary $ map readWord lns
    where
      readWord   ln = let (c:p:f) = words ln
                    in Word c p (splitCommas $ concat f)
      splitCommas w = let (b,e) = span (/= ',') w
                      in case e of
                          [] -> []
                          _  -> b : splitCommas (tail e)
      readLines h = do
        ln <- try (hGetLine h)
        case ln of
         Left (_ :: IOException) -> return []
         Right l                -> (l:) <$> readLines h

dictionary :: [ Word ] -> Dictionary
dictionary allWords = let numWords = length allWords
                      in  array (0,numWords -1) (zip [ 0 .. ] allWords)

randomWord :: StdGen -> Dictionary -> (Word, StdGen)
randomWord g dict = let (n, g') = randomR (0, length dict -1) g
                    in  (dict ! n, g')
