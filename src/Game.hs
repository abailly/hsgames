{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
module Game where

import           Data.Array
import           Data.List             (delete, find)
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set              as S
import           System.Random
import           System.Random.Shuffle

data ChainName = American | Continental | Festival | Imperial | Luxor | Tower | Worldwide
                deriving (Eq, Enum, Ord, Show, Read)

data HotelChain = HotelChain { chainName  :: ChainName
                             , chainTiles :: [ Tile ]
                             , chainStock :: Int
                             } deriving (Eq, Show, Read)

maximumStock = 25

stockPrice (HotelChain American    (length -> l) _ ) | l == 2  = 300
                                                    | l == 3  = 400
                                                    | l == 4  = 500
                                                    | l == 5  = 600
                                                    | l <= 10 = 700
                                                    | l <= 20 = 800
                                                    | l <= 30 = 900
                                                    | l <= 40 = 1000
                                                    | otherwise = 1100
stockPrice (HotelChain Worldwide l s) = stockPrice $ HotelChain American l s
stockPrice (HotelChain Festival l s)  = stockPrice $ HotelChain American l s

stockPrice (HotelChain Tower    (length -> l) _ ) | l == 2  = 200
                                                 | l == 3  = 300
                                                 | l == 4  = 400
                                                 | l == 5  = 500
                                                 | l <= 10 = 600
                                                 | l <= 20 = 700
                                                 | l <= 30 = 800
                                                 | l <= 40 = 900
                                                 | otherwise = 1000
stockPrice (HotelChain Luxor l s) = stockPrice $ HotelChain Tower l s

stockPrice (HotelChain Imperial    (length -> l) _ ) | l == 2  = 400
                                                    | l == 3  = 500
                                                    | l == 4  = 600
                                                    | l == 5  = 700
                                                    | l <= 10 = 800
                                                    | l <= 20 = 900
                                                    | l <= 30 = 1000
                                                    | l <= 40 = 1100
                                                    | otherwise = 1200
stockPrice (HotelChain Continental l s) = stockPrice $ HotelChain Imperial l s


data Content = Empty
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

type Coord = (Char,Int)

newtype Tile = Tile { tileCoords :: Coord } deriving (Eq, Show, Read, Ix, Ord)

data Cell = Cell { cellCoord   :: Tile
                 , cellContent :: Content
                 } deriving (Eq, Show ,Read)

data PlayerType = Human | Robot deriving (Eq, Show, Read)

data Player = Player { playerName :: String
                     , playerType :: PlayerType
                     , tiles      :: [ Tile ]
                     , ownedStock :: M.Map ChainName Int
                     , ownedCash  :: Int
                     } deriving (Eq, Show, Read)

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
                                                         adj = S.fromList $ map cellCoord $ adjacentCells (not . isEmpty . cellContent) board c
                                                         next = adj `S.difference` done
                                                     in buildLinked board next (c `S.insert` adj)
                                | otherwise       = S.foldl' (\ d c -> buildLinked board (S.singleton c) done `S.union` d) done todo

data Game = Game { gameBoard    :: GameBoard
                 , players      :: M.Map PlayerName Player
                 , drawingTiles :: [ Tile ]
                 , hotelChains  :: M.Map ChainName HotelChain
                 , turn         :: (PlayerName, Phase)
                 } deriving (Eq, Show, Read)

currentPlayer :: Game -> Player
currentPlayer game = let p = fst $ turn game
                     in players game M.! p

data Phase = PlaceTile
           | FundChain Tile
           | BuySomeStock Int
           deriving (Eq, Show, Read)

possiblePlay :: Game -> [ Order ]
possiblePlay (Game board plys _ chains (name, PlaceTile))           =  map (Place name) (tiles $ plys M.! name)
possiblePlay game@(Game board plys _ chains (name, FundChain t))    =  map (\ c -> Fund name c t) (filter (not . hasActiveChain game) $ M.keys chains)
possiblePlay game@(Game board plys _ chains (name, BuySomeStock 0)) =  []
possiblePlay game@(Game board plys _ chains (name, BuySomeStock n)) =  map (\ c -> BuyStock name c 1) (filter (hasActiveChain game) $ M.keys chains)

newGame :: StdGen -> Int -> Game
newGame g numTiles = Game initialBoard players (drop (2 * numTiles) coords) chains ("arnaud", PlaceTile)
  where
    initialBoard = array (Tile ('A',1),Tile ('I',12)) (map (\ cell@(Cell c _) -> (c, cell)) cells)
    coords       = shuffle' (indices initialBoard)  (9 * 12) g
    players      = M.fromList [ ("arnaud", Player "arnaud" Human (take numTiles coords) M.empty 6000)
                              , ("bernard", Player "bernard" Human (take numTiles $ drop numTiles coords) M.empty 6000)
                              ]
    cells        = concatMap (\ (cs,n) -> map (\ (r,e) -> Cell (Tile (n,r)) e) cs) rows
    rows         = zip (replicate 9 (take 12 cols)) [ 'A' .. ]
    cols         = zip [ 1 .. ] (repeat Empty)
    chains       = M.fromList $ map (\ n -> (n, HotelChain n [] maximumStock)) (enumFrom American)

type PlayerName = String

data Order = Place PlayerName Tile
           | Fund PlayerName ChainName Tile
           | BuyStock PlayerName ChainName Int
           | Cancel
           deriving (Eq, Show, Read)

play :: Game -> Order -> Game
play game          Cancel             = game
play game@Game{..} (BuyStock player chain qty) = buyStock game player chain qty
play game@Game{..} (Fund player chain coord) = if   gameBoard `hasNeutralChainAt` coord
                                               then createNewChain game player chain coord
                                               else game
play game (Place name coord)  = placeTile game name coord

nextPlayer :: Game -> PlayerName
nextPlayer game = let (p,_) = turn game
                  in case M.lookupGT p (players game) of
                      Nothing -> fst $ M.findMin (players game)
                      Just (p',_) -> p'

placeTile :: Game -> PlayerName -> Tile -> Game
placeTile  game@Game{..} name coord = let playableTile   = find ((== name) . playerName) (M.elems players) >>= find (== coord) . tiles
                                          removeTile t p = p { tiles = head drawingTiles : delete t (tiles p) }
                                      in case playableTile of
                                          Nothing   -> game
                                          Just tile -> let adj = linkedCells gameBoard tile
                                                           owners = catMaybes $ map (isOwned . cellContent) adj
                                                           expandChain c = c { chainTiles = map  cellCoord adj }
                                                       in case owners of
                                                           [] -> game { gameBoard = gameBoard // [ (tile, Cell tile (Neutral tile)) ]
                                                                      , drawingTiles = tail drawingTiles
                                                                      , players =  M.adjust (removeTile tile) name players
                                                                      , turn = (nextPlayer game, PlaceTile)
                                                                      }
                                                           (c:_) -> game { gameBoard = gameBoard // map (\ (Cell t _) -> (t, Cell t (Chain c))) adj
                                                                         , drawingTiles = tail drawingTiles
                                                                         , hotelChains = M.adjust expandChain c hotelChains
                                                                         , players =  M.adjust (removeTile tile) name players
                                                                         , turn = (nextPlayer game, PlaceTile)
                                                                         }


hasNeutralChainAt :: GameBoard -> Tile -> Bool
hasNeutralChainAt board coord = isNeutral (cellContent $ board ! coord) && hasAdjacentNeutralTile board coord

hasActiveChain :: Game -> ChainName -> Bool
hasActiveChain Game{..} chain = length (chainTiles (hotelChains M.! chain)) > 0

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
buyStock :: Game -> PlayerName -> ChainName -> Int -> Game
buyStock game@Game{..} player chain qty = if   game `hasActiveChain` chain            &&
                                               chainStock (hotelChains M.! chain) >= qty
                                          then let price = stockPrice (hotelChains M.! chain) * qty
                                                   decreaseStock c = c { chainStock = chainStock c - qty }
                                                   addOwnedStock (Just n) = Just $ n + qty
                                                   addOwnedStock Nothing  = Just qty
                                                   buyAndPayStock p = p { ownedCash = ownedCash p - price
                                                                        , ownedStock = M.alter addOwnedStock chain (ownedStock p)
                                                                        }
                                               in  if ownedCash (players M.! player) >= price
                                                   then game { hotelChains = M.adjust decreaseStock chain hotelChains
                                                             , players = M.adjust buyAndPayStock player players
                                                             }
                                                   else game
                                            else game
