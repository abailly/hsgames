{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
module Game where

import           Data.Array
import           Data.Function
import           Data.List             (delete, find, groupBy, nub, sort,
                                        sortBy)
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

type HotelChains = M.Map ChainName HotelChain

isActive :: HotelChain -> Bool
isActive HotelChain{..} = not (null chainTiles)

isSafe :: HotelChain -> Bool
isSafe HotelChain{..} = length chainTiles >= 11

isOverLimit :: HotelChain -> Bool
isOverLimit HotelChain{..} = length chainTiles >= 41

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

mergerBonus (HotelChain American    (length -> l) _ ) | l == 2     = (3000,1500)
                                                     | l == 3     = (4000,2000)
                                                     | l == 4     = (5000,2500)
                                                     | l == 5     = (6000,3000)
                                                     | l <= 10    = (7000,3500)
                                                     | l <= 20    = (8000,4000)
                                                     | l <= 30    = (9000,4500)
                                                     | l <= 40    = (10000,5000)
                                                     | otherwise = (11000,5500)
mergerBonus (HotelChain Worldwide l s) = mergerBonus $ HotelChain American l s
mergerBonus (HotelChain Festival l s)  = mergerBonus $ HotelChain American l s

mergerBonus (HotelChain Tower    (length -> l) _ ) | l == 2     = (2000,1000)
                                                  | l == 3     = (3000,1500)
                                                  | l == 4     = (4000,2000)
                                                  | l == 5     = (5000,2500)
                                                  | l <= 10    = (6000,3000)
                                                  | l <= 20    = (7000,3500)
                                                  | l <= 30    = (8000,4000)
                                                  | l <= 40    = (9000,4500)
                                                  | otherwise = (10000,5000)
mergerBonus (HotelChain Luxor l s) = mergerBonus $ HotelChain Tower l s

mergerBonus (HotelChain Imperial    (length -> l) _ ) | l == 2     = (4000,2000)
                                                     | l == 3     = (5000,2500)
                                                     | l == 4     = (6000,3000)
                                                     | l == 5     = (7000,3500)
                                                     | l <= 10    = (8000,4000)
                                                     | l <= 20    = (9000,4500)
                                                     | l <= 30    = (10000,5000)
                                                     | l <= 40    = (11000,5500)
                                                     | otherwise = (12000,6000)
mergerBonus (HotelChain Continental l s) = mergerBonus $ HotelChain Imperial l s

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
                                | otherwise       = S.foldl' (\ d c -> buildLinked board (S.singleton c) d) done todo

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
           | ResolveMerger MergerPhase Turn
           | GameEnds
           deriving (Eq, Show, Read)

type Turn = (PlayerName, Phase)

gameCanEnd :: HotelChains -> Bool
gameCanEnd chains = (not $ null $ activeChains chains) &&
                    (all isSafe (activeChains chains)     ||
                      any isOverLimit (activeChains chains))

activeChains chains = M.elems $ M.filter isActive chains

completeWithEndGame :: HotelChains -> [Order] -> [Order]
completeWithEndGame chains orders = if gameCanEnd chains
                                    then EndGame : orders
                                    else orders

possiblePlay :: Game -> [ Order ]
possiblePlay (Game _ _  _ _ (name, GameEnds))            =  [Cancel]
possiblePlay (Game board plys _ chains (name, PlaceTile))           =  completeWithEndGame chains $ map (Place name) (tiles $ plys M.! name)
possiblePlay (Game board plys _ chains (name, ResolveMerger (TakeOver tile [c1,c2]) _))
                                                                    =  trace ("merge "++ show c1 ++ ", " ++ show c2) $
                                                                       if length (chainTiles (chains M.! c1)) > length (chainTiles (chains M.! c2))
                                                                       then [Merge name tile c1 c2]
                                                                       else if length (chainTiles (chains M.! c1)) < length (chainTiles (chains M.! c2))
                                                                            then [Merge name tile c2 c1]
                                                                            else [Merge name tile c2 c1, Merge name tile c1 c2]
possiblePlay (Game _ plys _ _ (name, ResolveMerger (DisposeStock player buyer buyee price (next:pys)) _))
                                                                    =  case M.lookup buyee (ownedStock $ plys M.! next) of
                                                                        Nothing -> [Pass]
                                                                        Just n  ->
                                                                          Pass :
                                                                          [SellStock next buyee price k | k <- [ 1 .. n ] ] ++
                                                                          [ExchangeStock next buyer buyee k | k <- [ 1 .. n `div` 2 ] ]
possiblePlay game@(Game board plys _ chains (name, FundChain t))    =  let availableChainsForFounding = (filter (not . hasActiveChain game) $ M.keys chains)
                                                                       in if not (null availableChainsForFounding)
                                                                          then completeWithEndGame chains $
                                                                               map (\ c -> Fund name c t) availableChainsForFounding
                                                                          else [Pass]

possiblePlay game@(Game board plys _ chains (name, BuySomeStock n)) =  completeWithEndGame chains $
                                                                       Pass : (map (\ c -> BuyStock name c)                 $
                                                                               filter (hasEnoughMoneyToBuyStock name game) $
                                                                               filter (hasActiveChain game)                $
                                                                               M.keys chains)


hasEnoughMoneyToBuyStock :: PlayerName -> Game -> ChainName -> Bool
hasEnoughMoneyToBuyStock player game@Game{..} chain = let price = stockPrice (hotelChains M.! chain)
                                                      in ownedCash (players M.! player) >= price

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
           | SellStock PlayerName ChainName Int Int
           | ExchangeStock PlayerName ChainName ChainName Int
           | Pass
           | EndGame
           | Cancel
           deriving (Eq, Show, Read)

nextTurnInMergerSolving :: Game -> Turn -> Turn
nextTurnInMergerSolving game (_, ResolveMerger (DisposeStock player buyer buyee price (this:next:pys)) cont) =
  case M.lookup buyee (ownedStock $ (players game) M.! next) of
   Nothing -> (next, ResolveMerger (DisposeStock player buyer buyee price (next:pys)) cont)
   Just 0  -> (next, ResolveMerger (DisposeStock player buyer buyee price (next:pys)) cont)
   Just n  -> (this, ResolveMerger (DisposeStock player buyer buyee price (this:next:pys)) cont)
nextTurnInMergerSolving game (_, ResolveMerger (DisposeStock player buyer buyee price [this]) cont) =
  case M.lookup buyee (ownedStock $ (players game) M.! this) of
   Nothing -> cont
   Just 0  -> cont
   Just n  -> (this, ResolveMerger (DisposeStock player buyer buyee price [this]) cont)

play :: Game -> Order -> Game
play game          Cancel                    = game
play game          Pass                      = playerPass game
play game@Game{..} (BuyStock player chain)   = buyStock game player chain
play game@Game{..} (Merge player tile chain1 chain2)  = merge game player tile chain1 chain2
play game@Game{..} (SellStock player chain1 price qty) =
  case M.lookup chain1 (ownedStock $ players M.! player) of
   Nothing -> game { turn = nextTurnInMergerSolving game turn }
   Just n  -> let sellStock p = p { ownedStock = M.adjust (\ q -> q - qty) chain1 (ownedStock p)
                                  , ownedCash = ownedCash p + price * qty }
                  increaseStock c = c { chainStock = chainStock c + qty }
              in game { hotelChains = M.adjust increaseStock chain1 hotelChains
                      , players = M.adjust sellStock player players
                      , turn = nextTurnInMergerSolving game turn
                      }
play game@Game{..} (ExchangeStock player buyer buyee qty) =
  case M.lookup buyee (ownedStock $ players M.! player) of
   Nothing -> game { turn = nextTurnInMergerSolving game turn }
   Just n  -> let buyerRemainingStock = chainStock $ hotelChains M.! buyer
                  xchgedStock = min buyerRemainingStock qty
                  xchgStock p = p { ownedStock = M.adjust (+ xchgedStock) buyer $
                                                 M.adjust (\ k -> k - (xchgedStock * 2)) buyee (ownedStock p)
                                  }
                  increaseStock c = c { chainStock = chainStock c + (2 * xchgedStock) }
                  decreaseStock c = c { chainStock = chainStock c - xchgedStock }
              in game { hotelChains = M.adjust decreaseStock buyer $ M.adjust increaseStock buyee hotelChains
                      , players = M.adjust xchgStock player players
                      , turn = nextTurnInMergerSolving game turn
                      }
play game@Game{..} (Fund player chain coord) = if   gameBoard `hasNeutralChainAt` coord
                                               then createNewChain game player chain coord
                                               else game
play game (Place name coord)  = placeTile game name coord
play game@Game{..} EndGame    = let game' = foldl computeMergerBonus game (map chainName $ activeChains hotelChains)
                                    sellEverything p = p { ownedCash  = ownedCash p +
                                                                        (sum $ M.elems $ M.mapWithKey (\ k a -> stockPrice (hotelChains M.! k) * a) (ownedStock p))
                                                         , ownedStock = M.empty
                                                         }
                                in game' { players = M.map sellEverything players
                                         , turn = (fst turn, GameEnds) }

nextPlayer :: Game -> PlayerName
nextPlayer game = let (p,_) = turn game
                  in case M.lookupGT p (players game) of
                      Nothing -> fst $ M.findMin (players game)
                      Just (p',_) -> p'

placeTile :: Game -> PlayerName -> Tile -> Game
placeTile  game@Game{..} name coord = let isTilePlayable   = find ((== name) . playerName) (M.elems players) >>= find (== coord) . tiles
                                      in drawTile name isTilePlayable $ doPlayTile name isTilePlayable game

drawTile :: PlayerName -> Maybe Tile -> Game -> Game
drawTile name     Nothing game = game
drawTile name (Just tile) game@Game{..} = let removeTile t p = p { tiles = head drawingTiles : delete t (tiles p) }
                                          in game { drawingTiles = tail drawingTiles
                                                  , players      = M.adjust (removeTile tile) name players
                                                  }

buyStockOrNextPlayer name game@Game{..} = if   any (hasActiveChain game) (M.keys hotelChains)
                                          then (name, BuySomeStock 3)
                                          else (nextPlayer game, PlaceTile)

doPlayTile :: PlayerName -> Maybe Tile -> Game -> Game
doPlayTile _    Nothing     game@Game{..} = game
doPlayTile name (Just tile) game@Game{..} = let newCell = Cell tile (Neutral tile)
                                                adj = linkedCells gameBoard newCell
                                                owners = nub $ sort $ catMaybes $ map (isOwned . cellContent) adj
                                            in case owners of
                                                [] -> game { gameBoard = gameBoard // [ (tile, newCell) ]
                                                           , turn = if hasAdjacentNeutralTile gameBoard tile
                                                                    then (name, FundChain tile)
                                                                    else buyStockOrNextPlayer name game
                                                           }
                                                [c]     -> tileExpandsExistingChain c adj name game
                                                [c1,c2] -> twoChainsMerger c1 c2 tile name game
                                                chains  -> threeChainsMerger chains tile name game

tileExpandsExistingChain :: ChainName -> [Cell] -> PlayerName -> Game -> Game
tileExpandsExistingChain c adj name game@Game{..} = game { gameBoard = gameBoard // map (\ (Cell t _) -> (t, Cell t (Chain c))) adj
                                                         , hotelChains  = M.adjust expandChain c hotelChains
                                                         , turn         = buyStockOrNextPlayer name game
                                                         }
  where
    expandChain c = c { chainTiles = map cellCoord adj }

twoChainsMerger :: ChainName -> ChainName -> Tile -> PlayerName -> Game -> Game
twoChainsMerger c1 c2 tile name game@Game{..} = if not (isSafe (hotelChains M.! c1)) || not (isSafe (hotelChains M.! c2))
                                                then game { turn = (name, ResolveMerger (TakeOver tile [c1,c2]) ((nextPlayer game), PlaceTile)) }
                                                else game

threeChainsMerger :: [ChainName] -> Tile -> PlayerName -> Game -> Game
threeChainsMerger mergedChains tile name game@Game{..} = let mergedChainsBySize = sortBy (compare `on` (negate . length . chainTiles)) $ map (hotelChains M.!) mergedChains
                                                             largestChain = chainName $ head mergedChainsBySize
                                                             secondChain = chainName $ head $ tail mergedChainsBySize
                                                             smallestChain = chainName $ head $ tail $ tail mergedChainsBySize
                                                         in if any (not . isSafe) mergedChainsBySize
                                                            then game { turn = (name,
                                                                                ResolveMerger (TakeOver tile [largestChain,secondChain])
                                                                                (name, ResolveMerger (TakeOver tile [largestChain,smallestChain]) (nextPlayer game, PlaceTile)))
                                                                      }
                                                            else game

hasNeutralChainAt :: GameBoard -> Tile -> Bool
hasNeutralChainAt board coord = isNeutral (cellContent $ board ! coord) && hasAdjacentNeutralTile board coord

hasActiveChain :: Game -> ChainName -> Bool
hasActiveChain Game{..} chain = length (chainTiles (hotelChains M.! chain)) > 0

hasAdjacentNeutralTile :: GameBoard -> Tile -> Bool
hasAdjacentNeutralTile board coord = not (null (adjacentCells (isNeutral . cellContent) board coord))

createNewChain :: Game -> String -> ChainName -> Tile -> Game
createNewChain game@Game{..} player chain coord = let linked = linkedCells gameBoard (Cell coord (Neutral coord))
                                                      fundedChain c = c { chainTiles = map cellCoord linked, chainStock = chainStock c - 1 }
                                                      getFoundersShare Nothing  = Just 1
                                                      getFoundersShare (Just n) = Just $ n + 1
                                                      chainFounder p = p { ownedStock = M.alter getFoundersShare chain (ownedStock p) }
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
                                           in  if hasEnoughMoneyToBuyStock player game chain
                                               then game { hotelChains = M.adjust decreaseStock chain hotelChains
                                                         , players = M.adjust buyAndPayStock player players
                                                         , turn = case turn of
                                                                   (p, BuySomeStock n) | n > 1 -> (player, BuySomeStock (n-1))
                                                                   _                           -> (nextPlayer game, PlaceTile)
                                                         }
                                               else game
                                      else game

playerPass :: Game -> Game
playerPass game = game { turn = case turn game of
                                 (_, ResolveMerger (DisposeStock player buyer buyee price (this:next:pys)) cont)  ->
                                   (next, ResolveMerger (DisposeStock player buyer buyee price (next:pys)) cont)
                                 (_, ResolveMerger (DisposeStock player buyer buyee price [this]) cont)           ->
                                   cont
                                 _                                             -> (nextPlayer game, PlaceTile)
                       }

merge :: Game -> PlayerName -> Tile ->  ChainName -> ChainName -> Game
merge game@Game{..} name tile buyer buyee = let buyerChain = hotelChains M.! buyer
                                                buyeeChain = hotelChains M.! buyee
                                                mergedTiles =  tile : chainTiles buyerChain ++ chainTiles buyeeChain
                                                mergeIntoBuyer c = c { chainTiles = mergedTiles }
                                                clearBuyee c = c { chainTiles = [] }
                                                game' = computeMergerBonus game buyee
                                            in if length (chainTiles buyerChain) >=  length (chainTiles buyeeChain) &&
                                                  isActive buyerChain && isActive buyeeChain
                                               then game' { hotelChains = M.adjust clearBuyee buyee $ M.adjust mergeIntoBuyer buyer hotelChains
                                                          , gameBoard = gameBoard // map ( \ t -> (t, (Cell t (Chain buyer)))) mergedTiles
                                                          , turn = (head $ M.keys players,
                                                                    let (_, ResolveMerger _ cont) = turn
                                                                    in ResolveMerger
                                                                       (DisposeStock (nextPlayer game) buyer buyee (stockPrice buyeeChain) (M.keys players)) cont) }
                                               else game

computeMergerBonus :: Game -> ChainName -> Game
computeMergerBonus game@Game{..} chain = let buyeeChain = hotelChains M.! chain
                                             buyeeOwnedStock p = M.findWithDefault 0 chain (ownedStock p)
                                             plys = M.elems players
                                             shareHolders = groupBy ((==) `on` snd) $
                                                            sortBy (compare `on` (negate . snd)) $
                                                            filter ((/=0) . snd) $
                                                            zip plys (map buyeeOwnedStock plys)
                                         in case shareHolders of
                                             []            -> game
                                             [fsts]        -> divideAmong (map fst fsts) (uncurry (+) $ mergerBonus buyeeChain) game
                                             (fsts:snds:_) -> divideAmong (map fst snds) (snd $ mergerBonus buyeeChain) $
                                                              divideAmong (map fst fsts) (fst $ mergerBonus buyeeChain) game

divideAmong :: [Player] -> Int -> Game -> Game
divideAmong plys amount game@Game{..} = let bonus = amount `div` length plys
                                            playersWithBonus = map (\ p -> p { ownedCash = bonus + ownedCash p}) plys
                                        in game { players = M.fromList (zip (map playerName playersWithBonus) playersWithBonus) `M.union` players }
