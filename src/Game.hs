module Game(newGame, Game(..), Cell(..), Tile(..), HotelChain(..)) where

data HotelChain = American | Continental | Festival | Imperial | Luxor | Tower | Worldwide
                deriving (Eq, Enum, Show, Read)

data Tile = Empty
          | Neutral
          | Chain HotelChain
          deriving (Eq, Show, Read)

data Cell = Cell { cellCoord   :: (Char,Int)
                 , cellContent :: Tile
                 } deriving (Eq, Show ,Read)

data Game = Game { gameBoard :: [[ Cell ]] } deriving (Eq, Show, Read)

newGame :: Game
newGame = Game $ map (\ (cs,n) -> map (\ (r,e) -> Cell (n,r) e) cs) rows
  where
    rows = zip (replicate 9 (take 12 cols)) [ 'A' .. ]
    cols = zip [ 1 .. ] (repeat Empty)
