module Data.String.Extra

%access public export

parseInteger : List Char -> Integer -> Maybe Integer
parseInteger []        acc = Just acc
parseInteger (c :: cs) acc =
  if (c >= '0' && c <= '9')
  then parseInteger cs ((acc * 10) + (cast ((ord c) - (ord '0'))))
  else Nothing
