module Data.Maybe.Extra

public export
catMaybes : List (Maybe a) -> List a
catMaybes [] = []
catMaybes (Nothing :: rest) = catMaybes rest
catMaybes (Just x :: rest) = x :: catMaybes rest
