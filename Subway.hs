module Subway where

import Data.List(sort)

data Subway = Sub [Subway]
  deriving (Show, Eq, Ord)

fromString' :: [Subway] -> String -> Subway
fromString' [cur] [] = cur
fromString' (Sub ys : stack) ('0' : xs) =
  fromString' (Sub [] : (Sub (Sub [] : ys)) : stack) xs
fromString' (sub : (Sub (_ : ys)) : stack) ('1' : xs) =
  fromString' (Sub (sub : ys) : stack) xs
fromString' _ s = error $ "invalid Subway traversal: " <> s

fromString :: String -> Subway
fromString = fromString' [Sub []]

canonical :: Subway -> Subway
canonical (Sub ss) = Sub $ sort (map canonical ss)
