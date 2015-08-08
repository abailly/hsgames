{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
module Game where

import           Data.Array
import           Data.List             (delete, find, nub, sort)
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set              as S
import           Debug.Trace
import           System.Random
import           System.Random.Shuffle

data ChainName = American | Continental | Festival | Imperial | Luxor | Tower | Worldwide
                deriving (Eq, Enum, Ord, Show, Read)

data HotelChain = HotelChain { chainName  :: ChainName
                             , chainTiles :: [ Tile ]
                             , chainStock :: Int
                             } deriving (Eq, Show, Read)

isActive :: HotelChain -> Bool
isActive HotelChain{..} = not (null chainTiles)

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

instance Ord Cell where
  (Cell t _) `compare` (Cell t' _) = t `compare` t'

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

linkedCells :: GameBoard -> Cell -> [Cell]
linkedCells board coord = S.toList $ buildLinked board (S.singleton coord) S.empty
  where
    buildLinked :: GameBoard -> S.Set Cell -> S.Set Cell -> S.Set Cell
    buildLinked board todo done | S.null todo     = done
                                | S.size todo == 1 = let c = S.findMin todo
                                                         adj = S.fromList $ adjacentCells (not . isEmpty . cellContent) board (cellCoord c)
                                                         next = adj `S.difference` done
                                                     in buildLinked board next (c `S.insert` adj `S.union` done)
                                | otherwise       = S.foldl' (\ d c -> buildLinked board (S.singleton c) S.empty `S.union` d) done todo

data Game = Game { gameBoard    :: GameBoard
                 , players      :: M.Map PlayerName Player
                 , drawingTiles :: [ Tile ]
                 , hotelChains  :: M.Map ChainName HotelChain
                 , turn         :: (PlayerName, Phase)
                 } deriving (Eq, Show, Read)

currentPlayer :: Game -> Player
currentPlayer game = let p = fst $ turn game
                     in players game M.! p

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
           | ResolveMerger MergerPhase
           deriving (Eq, Show, Read)

possiblePlay :: Game -> [ Order ]
possiblePlay (Game board plys _ chains (name, PlaceTile))           =  map (Place name) (tiles $ plys M.! name)
possiblePlay (Game board plys _ chains (name, ResolveMerger (TakeOver tile [c1,c2])))
                                                                    =  if chainTiles (chains M.! c1) > chainTiles (chains M.! c1)
                                                                       then [Merge name tile c1 c2]
                                                                       else if chainTiles (chains M.! c1) < chainTiles (chains M.! c1)
                                                                            then [Merge name tile c2 c1]
                                                                            else [Merge name tile c2 c1, Merge name tile c1 c2]
possiblePlay (Game _ _ _ _ (name, ResolveMerger (DisposeStock player buyer buyee price (next:pys))))
                                                                    =  [ HoldStock next buyee, SellStock next buyee price, ExchangeStock next buyer buyee]
possiblePlay game@(Game board plys _ chains (name, FundChain t))    =  map (\ c -> Fund name c t) (filter (not . hasActiveChain game) $ M.keys chains)
possiblePlay game@(Game board plys _ chains (name, BuySomeStock n)) =  Pass : map (\ c -> BuyStock name c) (filter (hasActiveChain game) $ M.keys chains)

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
           | Merge PlayerName Tile ChainName ChainName
           | Fund PlayerName ChainName Tile
           | BuyStock PlayerName ChainName
           | SellStock PlayerName ChainName Int
           | HoldStock PlayerName ChainName
           | ExchangeStock PlayerName ChainName ChainName
           | Pass
           | Cancel
           deriving (Eq, Show, Read)

nextTurnInMergerSolving :: (PlayerName, Phase) -> (PlayerName, Phase)
nextTurnInMergerSolving (_, ResolveMerger (DisposeStock player buyer buyee price (this:next:pys))) =
  (next, ResolveMerger (DisposeStock player buyer buyee price (next:pys)))
nextTurnInMergerSolving (_, ResolveMerger (DisposeStock player buyer buyee price [this])) =
  (player, PlaceTile)

play :: Game -> Order -> Game
play game          Cancel                    = game
play game          Pass                      = game { turn = (nextPlayer game, PlaceTile) }
play game@Game{..} (BuyStock player chain)   = buyStock game player chain
play game@Game{..} (Merge player tile chain1 chain2)  = merge game player tile chain1 chain2
play game@Game{..} (HoldStock player chain1) = game { turn = nextTurnInMergerSolving turn }
play game@Game{..} (SellStock player chain1 price) =
  case M.lookup chain1 (ownedStock $ players M.! player) of
   Nothing -> game { turn = nextTurnInMergerSolving turn }
   Just n  -> let sellStock p = p { ownedStock = M.adjust (const 0) chain1 (ownedStock p)
                                  , ownedCash = ownedCash p + price * n }
                  increaseStock c = c { chainStock = chainStock c + n }
              in game { hotelChains = M.adjust increaseStock chain1 hotelChains
                      , players = M.adjust sellStock player players
                      , turn = nextTurnInMergerSolving turn
                      }
play game@Game{..} (ExchangeStock player buyer buyee) =
  case M.lookup buyee (ownedStock $ players M.! player) of
   Nothing -> game { turn = nextTurnInMergerSolving turn }
   Just n  -> let buyerRemainingStock = chainStock $ hotelChains M.! buyer
                  xchgedStock = min buyerRemainingStock ( n `div` 2)
                  xchgStock p = p { ownedStock = M.adjust (+ xchgedStock) buyer $
                                                 M.adjust (\ k -> k - (xchgedStock * 2)) buyee (ownedStock p)
                                  }
                  increaseStock c = c { chainStock = chainStock c + (2 * xchgedStock) }
                  decreaseStock c = c { chainStock = chainStock c - xchgedStock }
              in game { hotelChains = M.adjust decreaseStock buyer $ M.adjust increaseStock buyee hotelChains
                      , players = M.adjust xchgStock player players
                      , turn = nextTurnInMergerSolving turn
                      }
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
                                          buyStockOrNextPlayer = if   any (hasActiveChain game) (M.keys hotelChains)
                                                                 then (name, BuySomeStock 3)
                                                                 else (nextPlayer game, PlaceTile)
                                      in case playableTile of
                                          Nothing   -> game
                                          Just tile -> let newCell = Cell tile (Neutral tile)
                                                           adj = linkedCells gameBoard newCell
                                                           owners = nub $ sort $ catMaybes $ map (isOwned . cellContent) adj
                                                           expandChain c = c { chainTiles = map cellCoord adj }
                                                       in case owners of
                                                           [] -> game { gameBoard = gameBoard // [ (tile, newCell) ]
                                                                      , drawingTiles = tail drawingTiles
                                                                      , players =  M.adjust (removeTile tile) name players
                                                                      , turn = if hasAdjacentNeutralTile gameBoard tile
                                                                               then (name, FundChain tile)
                                                                               else buyStockOrNextPlayer
                                                                      }
                                                           -- Place a tile next to an existing chain
                                                           [c] -> game { gameBoard = gameBoard // map (\ (Cell t _) -> (t, Cell t (Chain c))) adj
                                                                       , drawingTiles = tail drawingTiles
                                                                       , hotelChains = M.adjust expandChain c hotelChains
                                                                       , players =  M.adjust (removeTile tile) name players
                                                                       , turn = buyStockOrNextPlayer
                                                                       }
                                                           -- Merger between 2 chains
                                                           [c1,c2] -> game { drawingTiles = tail drawingTiles
                                                                           , players =  M.adjust (removeTile tile) name players
                                                                           , turn = (name, ResolveMerger (TakeOver tile [c1,c2]))
                                                                           }


hasNeutralChainAt :: GameBoard -> Tile -> Bool
hasNeutralChainAt board coord = isNeutral (cellContent $ board ! coord) && hasAdjacentNeutralTile board coord

hasActiveChain :: Game -> ChainName -> Bool
hasActiveChain Game{..} chain = length (chainTiles (hotelChains M.! chain)) > 0

hasAdjacentNeutralTile :: GameBoard -> Tile -> Bool
hasAdjacentNeutralTile board coord = not (null (adjacentCells (isNeutral . cellContent) board coord))

createNewChain :: Game -> String -> ChainName -> Tile -> Game
createNewChain game@Game{..} player chain coord = let linked = linkedCells gameBoard (Cell coord (Neutral coord))
                                                      fundedChain c = c { chainTiles = map cellCoord linked, chainStock = chainStock c - 1 }
                                                      chainFounder p = p { ownedStock = M.insert chain 1 (ownedStock p) }
                                                  in  game { gameBoard  = gameBoard // map ( \ (Cell t _) -> (t, (Cell t (Chain chain)))) linked
                                                           , hotelChains = M.adjust fundedChain chain hotelChains
                                                           , players = M.adjust chainFounder player players
                                                           , turn = (player, BuySomeStock 3)
                                                           }
buyStock :: Game -> PlayerName -> ChainName -> Game
buyStock game@Game{..} player chain = if   game `hasActiveChain` chain            &&
                                           chainStock (hotelChains M.! chain) > 0
                                      then let price = stockPrice (hotelChains M.! chain)
                                               decreaseStock c = c { chainStock = chainStock c - 1 }
                                               addOwnedStock (Just n) = Just $ n + 1
                                               addOwnedStock Nothing  = Just 1
                                               buyAndPayStock p = p { ownedCash = ownedCash p - price
                                                                    , ownedStock = M.alter addOwnedStock chain (ownedStock p)
                                                                    }
                                           in  if ownedCash (players M.! player) >= price
                                               then game { hotelChains = M.adjust decreaseStock chain hotelChains
                                                         , players = M.adjust buyAndPayStock player players
                                                         , turn = case turn of
                                                                   (p, BuySomeStock n) | n > 1 -> (player, BuySomeStock (n-1))
                                                                   _                           -> (nextPlayer game, PlaceTile)
                                                         }
                                               else game
                                      else game

merge :: Game -> PlayerName -> Tile ->  ChainName -> ChainName -> Game
merge game@Game{..} name tile buyer buyee = let buyerChain = hotelChains M.! buyer
                                                buyeeChain = hotelChains M.! buyee
                                                mergedTiles =  tile : chainTiles buyerChain ++ chainTiles buyeeChain
                                                mergeIntoBuyer c = c { chainTiles = mergedTiles }
                                                clearBuyee c = c { chainTiles = [] }
                                            in if length (chainTiles buyerChain) >=  length (chainTiles buyeeChain) &&
                                                  isActive buyerChain && isActive buyeeChain
                                               then game { hotelChains = M.adjust clearBuyee buyee $ M.adjust mergeIntoBuyer buyer hotelChains
                                                         , gameBoard = gameBoard // map ( \ t -> (t, (Cell t (Chain buyer)))) mergedTiles
                                                         , turn = (head $ M.keys players,
                                                                   ResolveMerger $ DisposeStock (nextPlayer game) buyer buyee (stockPrice buyeeChain) (M.keys players)) }
                                               else game

