module Game(newGame, Game(..), Cell(..), Tile(..), Content(..), HotelChain(..), Order(..)) where

import           Data.Array

data HotelChain = American | Continental | Festival | Imperial | Luxor | Tower | Worldwide
                deriving (Eq, Enum, Show, Read)

data Content = Empty
             | Neutral Tile
             | Chain HotelChain
             deriving (Eq, Show, Read)

type Coord = (Char,Int)

newtype Tile = Tile { tileCoords :: Coord } deriving (Eq, Show, Read)

data Cell = Cell { cellCoord   :: Tile
                 , cellContent :: Content
                 } deriving (Eq, Show ,Read)

data Player = Player { playerName :: String
                     , tiles      :: [ Tile ] }
              deriving (Eq, Show, Read)

data Game = Game { gameBoard :: Array Coord Cell
                 , players   :: [ Player ]
                 } deriving (Eq, Show, Read)

newGame :: Game
newGame = Game initialBoard initialPlayers
  where
    initialPlayers = []
    initialBoard = array (('A',1),('I',12)) (map (\ cell@(Cell (Tile c) _) -> (c, cell)) cells)
    cells = concatMap (\ (cs,n) -> map (\ (r,e) -> Cell (Tile (n,r)) e) cs) rows
    rows = zip (replicate 9 (take 12 cols)) [ 'A' .. ]
    cols = zip [ 1 .. ] (repeat Empty)

data Order = Play String Tile deriving (Eq, Show, Read)

