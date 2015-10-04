module Acquire.Tiles where

import           Data.Array

type Coord = (Char,Int)

newtype Tile = Tile { tileCoords :: Coord } deriving (Eq, Show, Read, Ix, Ord)
