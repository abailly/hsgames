module Acquire.Cells where

import           Data.Array
import           Data.Maybe
import qualified Data.Set       as S

import           Acquire.Hotels
import           Acquire.Tiles

data Content = Empty
             | Playable  -- ^Used for highlighting purpose
             | Neutral Tile
             | Chain ChainName
             deriving (Eq, Show, Read)

isEmpty :: Content -> Bool
isEmpty Empty = True
isEmpty _     = False

isNeutral :: Content -> Bool
isNeutral (Neutral _) = True
isNeutral _           = False

isOwned :: Content -> Maybe ChainName
isOwned (Chain c) = Just c
isOwned _         = Nothing

data Cell = Cell { cellCoord   :: Tile
                 , cellContent :: Content
                 } deriving (Eq, Show ,Read)

instance Ord Cell where
  (Cell t _) `compare` (Cell t' _) = t `compare` t'

type GameBoard = Array Tile Cell

adjacentCells :: (Cell -> Bool) -> GameBoard -> Tile -> [Cell]
adjacentCells p board (Tile (x,y)) = let (Tile (lr,lc), Tile (ur,uc)) = bounds board
                                     in filter p $ map (board !) $ catMaybes [ if x > lr then Just (Tile (pred x,y)) else Nothing
                                                                             , if x < ur then Just (Tile (succ x,y)) else Nothing
                                                                             , if y > lc then Just (Tile (x,pred y)) else Nothing
                                                                             , if y < uc then Just (Tile (x,succ y)) else Nothing
                                                                             ]

linkedCells :: GameBoard -> Cell -> [Cell]
linkedCells gameBoard coord = S.toList $ buildLinked gameBoard (S.singleton coord) S.empty
  where
    buildLinked :: GameBoard -> S.Set Cell -> S.Set Cell -> S.Set Cell
    buildLinked board todo done | S.null todo     = done
                                | S.size todo == 1 = let c = S.findMin todo
                                                         adj = S.fromList $ adjacentCells (not . isEmpty . cellContent) board (cellCoord c)
                                                         next = adj `S.difference` done
                                                     in buildLinked board next (c `S.insert` adj `S.union` done)
                                | otherwise       = S.foldl' (\ d c -> buildLinked board (S.singleton c) d) done todo

