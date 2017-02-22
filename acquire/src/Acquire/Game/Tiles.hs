{-# LANGUAGE OverloadedStrings #-}
module Acquire.Game.Tiles where

import           Data.Aeson
import           Data.Array

type Coord = (Char,Int)

newtype Tile = Tile { tileCoords :: Coord }
  deriving (Eq, Show, Read, Ix, Ord)

instance ToJSON Tile where
  toJSON (Tile (c,i)) = object [ "row" .= toJSON c
                               , "col" .= toJSON i
                               ]

instance FromJSON Tile where
  parseJSON (Object o) = Tile <$>
                         ((,)         <$>
                          o .: "row"  <*>
                          o .: "col")
  parseJSON v          = fail $ "Cannot parse Tile from JSON " ++ show v
