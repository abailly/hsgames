{-# LANGUAGE RecordWildCards #-}
module Game(newGame, Game(..), Cell(..), Tile(..), Content(..), ChainName(..), Order(..), Player(..), play) where

import           Data.Array
import           Data.List             (delete, find)
import qualified Data.Map              as M
import           Data.Maybe
import qualified Data.Set              as S
import           System.Random
import           System.Random.Shuffle

data ChainName = American | Continental | Festival | Imperial | Luxor | Tower | Worldwide
                deriving (Eq, Enum, Ord, Show, Read)

data HotelChain = HotelChain { chainTiles :: [ Tile ]
                             , chainStock :: Int
                             } deriving (Eq, Show, Read)

maximumStock = 25

data Content = Empty
             | Neutral Tile
             | Chain ChainName
             deriving (Eq, Show, Read)

isNeutral :: Content -> Bool
isNeutral (Neutral _) = True
isNeutral _           = False

type Coord = (Char,Int)

newtype Tile = Tile { tileCoords :: Coord } deriving (Eq, Show, Read, Ix, Ord)

data Cell = Cell { cellCoord   :: Tile
                 , cellContent :: Content
                 } deriving (Eq, Show ,Read)

data Player = Player { playerName :: String
                     , tiles      :: [ Tile ]
                     , ownedStock :: M.Map ChainName Int
                     }
              deriving (Eq, Show, Read)

type GameBoard = Array Tile Cell

adjacentCells :: (Cell -> Bool) -> GameBoard -> Tile -> [Cell]
adjacentCells p board (Tile (x,y)) = let (Tile (lr,lc), Tile (ur,uc)) = bounds board
                                     in filter p $ map (board !) $ catMaybes [ if x > lr then Just (Tile (pred x,y)) else Nothing
                                                                             , if x < ur then Just (Tile (succ x,y)) else Nothing
                                                                             , if y > lc then Just (Tile (x,pred y)) else Nothing
                                                                             , if y < uc then Just (Tile (x,succ y)) else Nothing
                                                                             ]

linkedCells :: GameBoard -> Tile -> [Cell]
linkedCells board coord = map (board !) $ S.toList $ buildLinked board (S.singleton coord) S.empty
  where
    buildLinked :: GameBoard -> S.Set Tile -> S.Set Tile -> S.Set Tile
    buildLinked board todo done | S.null todo     = done
                                | S.size todo == 1 = let c = S.findMin todo
                                                         adj = S.fromList $ map cellCoord $ adjacentCells (isNeutral . cellContent) board c
                                                         next = adj `S.difference` done
                                                     in buildLinked board next (c `S.insert` adj)
                                | otherwise       = S.foldl' (\ d c -> buildLinked board (S.singleton c) done `S.union` d) done todo

data Game = Game { gameBoard    :: GameBoard
                 , players      :: M.Map PlayerName Player
                 , drawingTiles :: [ Tile ]
                 , hotelChains  :: M.Map ChainName HotelChain
                 } deriving (Show, Read)

newGame :: StdGen -> Game
newGame g = Game initialBoard players (drop 6 coords) chains
  where
    initialBoard = array (Tile ('A',1),Tile ('I',12)) (map (\ cell@(Cell c _) -> (c, cell)) cells)
    coords       = shuffle' (indices initialBoard)  (9 * 12) g
    players      = M.fromList [ ("arnaud", Player "arnaud" (take 6 coords) M.empty) ]
    cells        = concatMap (\ (cs,n) -> map (\ (r,e) -> Cell (Tile (n,r)) e) cs) rows
    rows         = zip (replicate 9 (take 12 cols)) [ 'A' .. ]
    cols         = zip [ 1 .. ] (repeat Empty)
    chains       = M.fromList $ map (\ n -> (n, HotelChain [] maximumStock)) (enumFrom American)

type PlayerName = String

data Order = Place PlayerName Tile
           | Fund PlayerName ChainName Tile
           | Cancel
           deriving (Eq, Show, Read)

play :: Game -> Order -> Game
play game          Cancel             = game
play game@Game{..} (Fund player chain coord) = if   gameBoard `hasNeutralChainAt` coord
                                               then createNewChain game player chain coord
                                               else game
play game@Game{..} (Place name coord)  = let played         = find ((== name) . playerName) (M.elems players) >>= find (== coord) . tiles
                                             removeTile t p = p { tiles = delete t (tiles p) }
                                         in case played of
                                             Nothing   -> game
                                             Just tile -> game { gameBoard = gameBoard // [ (tile, Cell tile (Neutral tile)) ]
                                                               , players =  M.adjust (removeTile tile) name players
                                                               }

hasNeutralChainAt :: GameBoard -> Tile -> Bool
hasNeutralChainAt board coord = isNeutral (cellContent $ board ! coord) && hasAdjacentNeutralTile board coord

hasAdjacentNeutralTile :: GameBoard -> Tile -> Bool
hasAdjacentNeutralTile board coord = not (null (adjacentCells (isNeutral . cellContent) board coord))

createNewChain :: Game -> String -> ChainName -> Tile -> Game
createNewChain game@Game{..} player chain coord = let linked = linkedCells gameBoard coord
                                                      fundedChain c = c { chainTiles = map cellCoord linked, chainStock = chainStock c - 1 }
                                                      chainFounder p = p { ownedStock = M.insert chain 1 (ownedStock p) }
                                                  in  game { gameBoard  = gameBoard // map ( \ (Cell t _) -> (t, (Cell t (Chain chain)))) linked
                                                           , hotelChains = M.adjust fundedChain chain hotelChains
                                                           , players = M.adjust chainFounder player players
                                                           }
