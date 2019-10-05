module Data.Strings.Extra

import Data.Maybe
import Data.List

public export
parseInteger : List Char -> Integer -> Maybe Integer
parseInteger []        acc = Just acc
parseInteger (c :: cs) acc =
  if (c >= '0' && c <= '9')
  then parseInteger cs ((acc * 10) + ((cast c) - (cast '0')))
  else Nothing

||| Joins the character lists by newlines into a single character list.
|||
||| ```idris example
||| unlines' [['l','i','n','e'], ['l','i','n','e','2'], ['l','n','3'], ['D']]
||| ```
public export
unlines' : List (List Char) -> List Char
unlines' [] = []
unlines' (l::ls) = l ++ '\n' :: unlines' ls

||| Joins the strings by newlines into a single string.
|||
||| ```idris example
||| unlines ["line", "line2", "ln3", "D"]
||| ```
public export
unlines : List String -> String
unlines = pack . unlines' . map unpack
