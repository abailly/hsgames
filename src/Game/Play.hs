{-# LANGUAGE RecordWildCards #-}
module Game.Play where

import           Data.Array
import           Data.Function
import           Data.List     (delete, find, groupBy, nub, sort, sortBy)
import qualified Data.Map      as M
import           Data.Maybe

import           Cells
import           Hotels
import           Player
import           Tiles

import           Game.Core
import           Game.Turn

play :: Game -> Order -> Game
play game          Cancel                    = game
play game          Pass                      = playerPass game
play game@Game{..} (BuyStock player chain)   = buyStock game player chain
play game@Game{..} (Merge player tile chain1 chain2)  = merge game player tile chain1 chain2
play game@Game{..} (SellStock player chain1 price qty) =
  case M.lookup chain1 (ownedStock $ players M.! player) of
   Nothing -> game { turn = nextTurnInMergerSolving game turn }
   Just{}  -> let sellStock p = p { ownedStock = M.adjust (\ q -> q - qty) chain1 (ownedStock p)
                                  , ownedCash = ownedCash p + price * qty }
                  increaseStock c = c { chainStock = chainStock c + qty }
              in game { hotelChains = M.adjust increaseStock chain1 hotelChains
                      , players = M.adjust sellStock player players
                      , turn = nextTurnInMergerSolving game turn
                      }
play game@Game{..} (ExchangeStock player buyer buyee qty) =
  case M.lookup buyee (ownedStock $ players M.! player) of
   Nothing -> game { turn = nextTurnInMergerSolving game turn }
   Just{}  -> let buyerRemainingStock = chainStock $ hotelChains M.! buyer
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

placeTile :: Game -> PlayerName -> Tile -> Game
placeTile  game@Game{..} name coord = let isTilePlayable   = find ((== name) . playerName) (M.elems players) >>= find (== coord) . tiles
                                      in drawTile name isTilePlayable $ doPlayTile name isTilePlayable game

drawTile :: PlayerName -> Maybe Tile -> Game -> Game
drawTile _     Nothing game = game
drawTile name (Just tile) game@Game{..} = let removeTile t p = p { tiles = head drawingTiles : delete t (tiles p) }
                                          in game { drawingTiles = tail drawingTiles
                                                  , players      = M.adjust (removeTile tile) name players
                                                  }

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
tileExpandsExistingChain chainName adj name game@Game{..} = game { gameBoard = gameBoard // map (\ (Cell t _) -> (t, Cell t (Chain chainName))) adj
                                                                 , hotelChains  = M.adjust expandChain chainName hotelChains
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
                                           in  if hasEnoughMoneyToBuyStock (players M.! player) (hotelChains M.! chain)
                                               then game { hotelChains = M.adjust decreaseStock chain hotelChains
                                                         , players = M.adjust buyAndPayStock player players
                                                         , turn = case turn of
                                                                   (_, BuySomeStock n) | n > 1 -> (player, BuySomeStock (n-1))
                                                                   _                           -> (nextPlayer game, PlaceTile)
                                                         }
                                               else game
                                      else game

playerPass :: Game -> Game
playerPass game = game { turn = case turn game of
                                 (_, ResolveMerger (DisposeStock player buyer buyee price (_:next:pys)) cont)  ->
                                   (next, ResolveMerger (DisposeStock player buyer buyee price (next:pys)) cont)
                                 (_, ResolveMerger (DisposeStock _ _ _  _ [_]) cont)           ->
                                   cont
                                 _                                             -> (nextPlayer game, PlaceTile)
                       }

merge :: Game -> PlayerName -> Tile ->  ChainName -> ChainName -> Game
merge game@Game{..} _    tile buyer buyee = let buyerChain = hotelChains M.! buyer
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
