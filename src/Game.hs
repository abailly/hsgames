{-# LANGUAGE RecordWildCards #-}
module Game(newGame, Game(..), Cell(..), Tile(..), Content(..), HotelChain(..), Order(..), Player(..), play) where

import           Data.Array
import           Data.List             (delete, find)
import qualified Data.Map              as M
import           Data.Maybe
import qualified Data.Set              as S
import           System.Random
import           System.Random.Shuffle

data HotelChain = American | Continental | Festival | Imperial | Luxor | Tower | Worldwide
                deriving (Eq, Enum, Show, Read)

data Content = Empty
             | Neutral Tile
             | Chain HotelChain
             deriving (Eq, Show, Read)

isNeutral :: Content -> Bool
isNeutral (Neutral _) = True
isNeutral _           = False

type Coord = (Char,Int)

newtype Tile = Tile { tileCoords :: Coord } deriving (Eq, Show, Read)

data Cell = Cell { cellCoord   :: Tile
                 , cellContent :: Content
                 } deriving (Eq, Show ,Read)

data Player = Player { playerName :: String
                     , tiles      :: [ Tile ] }
              deriving (Eq, Show, Read)

type GameBoard = Array Coord Cell

adjacentCells :: (Cell -> Bool) -> GameBoard -> Coord -> [Cell]
adjacentCells p board (x,y) = let ((lr,lc),(ur,uc)) = bounds board
                                 in filter p $ map (board !) $ catMaybes [ if x > lr then Just (pred x,y) else Nothing
                                                                         , if x < ur then Just (succ x,y) else Nothing
                                                                         , if y > lc then Just (x,pred y) else Nothing
                                                                         , if y < uc then Just (x,succ y) else Nothing
                                                                         ]

linkedCells :: GameBoard -> Coord -> [Cell]
linkedCells board coord = map (board !) $ S.toList $ buildLinked board (S.singleton coord) S.empty
  where
    buildLinked :: GameBoard -> S.Set Coord -> S.Set Coord -> S.Set Coord
    buildLinked board todo done | S.null todo     = done
                                | S.size todo == 1 = let c = S.findMin todo
                                                         adj = S.fromList $ map (tileCoords . cellCoord) $ adjacentCells (isNeutral . cellContent) board c
                                                         next = adj `S.difference` done
                                                     in buildLinked board next (c `S.insert` adj)
                                | otherwise       = S.foldl' (\ d c -> buildLinked board (S.singleton c) done `S.union` d) done todo

data Game = Game { gameBoard    :: GameBoard
                 , players      :: M.Map String Player
                 , drawingTiles :: [ Tile ]
                 , hotelChains  :: [ HotelChain ]
                 } deriving (Show, Read)

newGame :: StdGen -> Game
newGame g = Game initialBoard players (drop 6 coords) (enumFrom American)
  where
    initialBoard = array (('A',1),('I',12)) (map (\ cell@(Cell (Tile c) _) -> (c, cell)) cells)
    coords       = map Tile $ shuffle' (indices initialBoard)  (9 * 12) g
    players      = M.fromList [ ("arnaud", Player "arnaud" (take 6 coords)) ]
    cells        = concatMap (\ (cs,n) -> map (\ (r,e) -> Cell (Tile (n,r)) e) cs) rows
    rows         = zip (replicate 9 (take 12 cols)) [ 'A' .. ]
    cols         = zip [ 1 .. ] (repeat Empty)

data Order = Play String Coord
           | Fund HotelChain Coord
           | Cancel
           deriving (Eq, Show, Read)

play :: Game -> Order -> Game
play game          Cancel             = game
play game@Game{..} (Fund chain coord) = if   gameBoard `hasNeutralChainAt` coord
                                        then createNewChain game chain coord
                                        else game
play game@Game{..} (Play name coord)  = let played         = find ((== name) . playerName) (M.elems players) >>= find ((== coord) . tileCoords) . tiles
                                            removeTile t p = p { tiles = delete t (tiles p) }
                                        in case played of
                                            Nothing   -> game
                                            Just tile -> game { gameBoard = gameBoard // [ (tileCoords tile, Cell tile (Neutral tile)) ]
                                                              , players =  M.adjust (removeTile tile) name players
                                                              }

hasNeutralChainAt :: GameBoard -> Coord -> Bool
hasNeutralChainAt board coord = isNeutral (cellContent $ board ! coord) && hasAdjacentNeutralTile board coord

hasAdjacentNeutralTile :: GameBoard -> Coord -> Bool
hasAdjacentNeutralTile board coord = not (null (adjacentCells (isNeutral . cellContent) board coord))

createNewChain :: Game -> HotelChain -> Coord -> Game
createNewChain game@Game{..} chain coord = let linked = linkedCells gameBoard coord
                                           in  game { gameBoard  = gameBoard // map ( \ (Cell t _) -> (tileCoords t, (Cell t (Chain chain)))) linked
                                                    , hotelChains = delete chain hotelChains
                                                    }
