{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Acquire.Game.Core where

import           Data.Aeson            (ToJSON)
import           Data.Array
import qualified Data.Map              as M
import           GHC.Generics
import           System.Random
import           System.Random.Shuffle

import           Acquire.Game.Cells
import           Acquire.Game.Hotels
import           Acquire.Game.Player
import           Acquire.Game.Tiles


data MergerPhase = TakeOver Tile [ChainName]
                 | DisposeStock { initialPlayer   :: PlayerName
                                , buyerChain      :: ChainName
                                , buyeeChain      :: ChainName
                                , buyeePrice      :: Int
                                , playersToDecide :: [PlayerName]
                                }
                 deriving (Eq,Show,Read,Generic)

instance ToJSON MergerPhase

data Phase = PlaceTile
           | FundChain Tile
           | BuySomeStock Int
           | ResolveMerger MergerPhase Turn
           | GameEnds
           deriving (Eq, Show, Read, Generic)

instance ToJSON Phase

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
           deriving (Eq, Show, Read, Generic)

instance ToJSON Order

type GameId = String

data Game = Game { gameId       :: GameId
                 , gameBoard    :: GameBoard
                 , players      :: Players
                 , drawingTiles :: [ Tile ]
                 , hotelChains  :: HotelChains
                 , turn         :: Turn
                 } deriving (Eq, Show, Read, Generic)

instance ToJSON Game

numberOfTilesPerPlayer :: Int
numberOfTilesPerPlayer = 6

newGame :: GameId -> StdGen -> [(PlayerName,PlayerType)] -> Game
newGame gid g playersDescription = Game gid initialBoard (M.fromList players) draw chains (firstPlayerName, PlaceTile)
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
makePlayers ((pname,ptype):rest) (ps,coords) =
  makePlayers rest ((pname, Player pname ptype (take numberOfTilesPerPlayer coords) emptyStock 6000):ps, drop numberOfTilesPerPlayer coords)
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
                      Nothing     -> fst $ M.findMin (players game)
                      Just (p',_) -> p'

