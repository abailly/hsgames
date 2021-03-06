{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chinese.Dictionary where

import           Control.Exception
import           Data.Array
import           Data.Char
import           Data.Maybe
import           Prelude                      hiding (Word)
import           System.IO
import           System.Random
import           Text.ParserCombinators.ReadP

-- | A dictionary entry for a chinese word
data Word = Word { chinese :: String
                 , pinyin  :: String
                 , french  :: [String]
                 }
            deriving Show

type Dictionary = Array Int Word

-- TODO use parsec
readDictionary :: FilePath -> IO Dictionary
readDictionary fname = withFile fname ReadMode $ \ h -> do
  hSetBuffering h NoBuffering
  lns <- readLines h
  return $ dictionary $ catMaybes $ map tryReadWord lns
    where
      tryReadWord s =
        case readP_to_S readWord s of
         (r,_):_ -> Just r
         _       -> Nothing

      notSpace = not . isSpace
      notComma = (/= ',')
      comma = satisfy (== ',')
      readWord = do
        zh <- munch1 notSpace
        skipSpaces
        py <- munch1 notSpace
        skipSpaces
        frs <- sepBy1 (munch1 notComma) comma
        return $ Word zh py frs
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
