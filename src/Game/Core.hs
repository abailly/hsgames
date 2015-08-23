{-# LANGUAGE RecordWildCards #-}
module Game.Core where

import           Data.Array
import qualified Data.Map              as M
import           System.Random
import           System.Random.Shuffle

import           Cells
import           Hotels
import           Player
import           Tiles


data MergerPhase = TakeOver Tile [ChainName]
                 | DisposeStock { initialPlayer   :: PlayerName
                                , buyerChain      :: ChainName
                                , buyeeChain      :: ChainName
                                , buyeePrice      :: Int
                                , playersToDecide :: [PlayerName]
                                }
                 deriving (Eq,Show,Read)

data Phase = PlaceTile
           | FundChain Tile
           | BuySomeStock Int
           | ResolveMerger MergerPhase Turn
           | GameEnds
           deriving (Eq, Show, Read)

type Turn = (PlayerName, Phase)

data Order = Place PlayerName Tile
           | Merge PlayerName Tile ChainName ChainName
           | Fund PlayerName ChainName Tile
           | BuyStock PlayerName ChainName
           | SellStock PlayerName ChainName Int Int
           | ExchangeStock PlayerName ChainName ChainName Int
           | Pass
           | EndGame
           | Cancel
           deriving (Eq, Show, Read)

data Game = Game { gameBoard    :: GameBoard
                 , players      :: Players
                 , drawingTiles :: [ Tile ]
                 , hotelChains  :: HotelChains
                 , turn         :: Turn
                 } deriving (Eq, Show, Read)

numberOfTilesPerPlayer :: Int
numberOfTilesPerPlayer = 6

newGame :: StdGen -> [(PlayerName,PlayerType)] -> Game
newGame g playersDescription = Game initialBoard (M.fromList players) draw chains (firstPlayerName, PlaceTile)
  where
    initialBoard = array (Tile ('A',1),Tile ('I',12)) (map (\ cell@(Cell c _) -> (c, cell)) cells)
    coords       = shuffle' (indices initialBoard)  (9 * 12) g
    (players, draw) = makePlayers playersDescription ([],coords)
    firstPlayerName = fst $ head playersDescription
    cells        = concatMap (\ (cs,n) -> map (\ (r,e) -> Cell (Tile (n,r)) e) cs) rows
    rows         = zip (replicate 9 (take 12 cols)) [ 'A' .. ]
    cols         = zip [ 1 .. ] (repeat Empty)
    chains       = M.fromList $ map (\ n -> (n, HotelChain n [] maximumStock)) (enumFrom American)

makePlayers :: [(PlayerName,PlayerType)] -> ([(PlayerName, Player)],[Tile]) -> ([(PlayerName, Player)],[Tile])
makePlayers ((pname,ptype):rest) (ps,coords) = makePlayers rest ((pname, Player pname ptype (take numberOfTilesPerPlayer coords) M.empty 6000):ps, drop numberOfTilesPerPlayer coords)
makePlayers [] res = res

currentPlayer :: Game -> Player
currentPlayer game = let p = fst $ turn game
                     in players game M.! p


hasNeutralChainAt :: GameBoard -> Tile -> Bool
hasNeutralChainAt board coord = isNeutral (cellContent $ board ! coord) && hasAdjacentNeutralTile board coord

hasActiveChain :: Game -> ChainName -> Bool
hasActiveChain Game{..} chain = length (chainTiles (hotelChains M.! chain)) > 0

hasAdjacentNeutralTile :: GameBoard -> Tile -> Bool
hasAdjacentNeutralTile board coord = not (null (adjacentCells (isNeutral . cellContent) board coord))

nextPlayer :: Game -> PlayerName
nextPlayer game = let (p,_) = turn game
                  in case M.lookupGT p (players game) of
                      Nothing -> fst $ M.findMin (players game)
                      Just (p',_) -> p'

