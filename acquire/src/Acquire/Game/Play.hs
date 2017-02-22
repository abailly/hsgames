{-# LANGUAGE RecordWildCards #-}
module Acquire.Game.Play where

import           Data.Array
import           Data.Function
import           Data.List           (delete, find, groupBy, nub, sort, sortBy)
import qualified Data.Map            as M
import           Data.Maybe

import           Acquire.Game.Cells
import           Acquire.Game.Core
import           Acquire.Game.Hotels
import           Acquire.Game.Player
import           Acquire.Game.Tiles
import           Acquire.Game.Turn

-- |
-- = Player turn
-- Each player’s turn consists of:
--
--  1. Placing one of his hotel tiles on its matching square on the board. see 'placeTile'
--  2. Buying Stock. see 'buyStock'
--  3. Drawing another hotel tile to replace the one played. See 'drawTile'
play :: Game -> Order -> Game
play game          Cancel                    = game
play game          Pass                      = playerPass game
play game@Game{..} (BuyStock player chain)   = buyStock game player chain
play game@Game{..} (Merge player tile chain1 chain2)  = merge game player tile chain1 chain2
play game@Game{..} (SellStock player chain1 price qty) = sellStock game player chain1 price qty
play game@Game{..} (ExchangeStock player buyer buyee qty) = exchangeStock game player buyer buyee qty
play game@Game{..} (Fund player chain coord) = if   gameBoard `hasNeutralChainAt` coord
                                               then createNewChain game player chain coord
                                               else game
play game          (Place name coord)        = placeTile game name coord
play game@Game{..} EndGame                   = endGame game


-- |
-- If this hotel creates a chain or causes a merger, all transactions connected with Creating Chains
-- or Merging Chains are completed before the player’s turn continues.
placeTile :: Game -> PlayerName -> Tile -> Game
placeTile  game@Game{..} name coord = let isTilePlayable   = find ((== name) . playerName) (M.elems players) >>= find (== coord) . tiles
                                      in drawTile name isTilePlayable $ doPlayTile name isTilePlayable game

drawTile :: PlayerName -> Maybe Tile -> Game -> Game
drawTile _     Nothing game = game
drawTile name (Just tile) game@Game{..}
  | null drawingTiles = game
  | otherwise         = let removeTile t p = p { tiles = head drawingTiles : delete t (tiles p) }
                        in game { drawingTiles = tail drawingTiles
                                , players      = M.adjust (removeTile tile) name players
                                }

-- |
doPlayTile :: PlayerName -> Maybe Tile -> Game -> Game
doPlayTile _    Nothing     game@Game{..} = game
doPlayTile name (Just tile) game@Game{..} =
  let newCell = Cell tile (Neutral tile)
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
tileExpandsExistingChain chainName adj name game@Game{..} =
  game { gameBoard = gameBoard // map (\ (Cell t _) -> (t, Cell t (Chain chainName))) adj
       , hotelChains  = M.adjust expandChain chainName hotelChains
       , turn         = buyStockOrNextPlayer name game
       }
  where
    expandChain c = c { chainTiles = map cellCoord adj }


-- |
-- == Creating new chains
--
-- When a player places a hotel tile next to one already on the board (rank or file—not diagonally), a chain is created.
-- For example, hotel tiles A1 and B2 are on the board but do not form a chain as they adjoin diagonally (not rank or file).
-- Assume that among his six hotels, a player has C2 and B1.  He places C2, creating a two-hotel chain.
-- (If he had placed B1, he would have created a three-hotel chain; see above diagram.)  Then assume that, on his next turn,
-- he places B1, expanding the chain to four hotels.
--
-- The maker of a chain chooses its name from seven possible chains, taking the appropriate chain marker and placing it
-- atop any one hotel in the chain.  He then receives a "Founder’s Bonus" of one free stock in that chain.
-- A maximum of seven chains may be on the board at one time.  Any hotel which would create an eighth chain may not be placed.
--
createNewChain :: Game -> String -> ChainName -> Tile -> Game
createNewChain game@Game{..} player chain coord =
  let linked = linkedCells gameBoard (Cell coord (Neutral coord))
      fundedChain c = c { chainTiles = map cellCoord linked, chainStock = chainStock c - 1 }
      getFoundersShare Nothing  = Just 1
      getFoundersShare (Just n) = Just $ n + 1
      chainFounder p = p { ownedStock = alterStock getFoundersShare chain (ownedStock p) }
  in  game { gameBoard  = gameBoard // map ( \ (Cell t _) -> (t, (Cell t (Chain chain)))) linked
           , hotelChains = M.adjust fundedChain chain hotelChains
           , players = M.adjust chainFounder player players
           , turn = (player, BuySomeStock 3)
           }

-- |
-- == Buying Stock
--
-- Any player, immediately after playing his hotel, may buy stock in any active chain—up to a maximum of three blocks
-- in any one turn.  If he is unable to place any of his hotels, he may still buy stock.  His purchase may be in one,
-- two or three different chains.  Price per block varies with the number of hotels in the chain.
--
-- A player who runs out of money cannot buy stock but must place a hotel if he is able.  He cannot raise money
-- by selling stock except during the designated disposal period after a merger.  Trading and selling of stock
-- between players is not permitted.  At any time, player may ask how much stock remains in a particular chain.
buyStock :: Game -> PlayerName -> ChainName -> Game
buyStock game@Game{..} player chain =
  let price                  = stockPrice (hotelChains M.! chain)
      decreaseStock c        = c { chainStock = chainStock c - 1 }
      addOwnedStock (Just n) = Just $ n + 1
      addOwnedStock Nothing  = Just 1
      buyAndPayStock p       = p { ownedCash = ownedCash p - price
                                 , ownedStock = alterStock addOwnedStock chain (ownedStock p)
                                 }
  in  if hasEnoughMoneyToBuyStock (players M.! player) (hotelChains M.! chain) &&
         game `hasActiveChain` chain                                           &&
         chainStock (hotelChains M.! chain) > 0
      then game { hotelChains = M.adjust decreaseStock chain hotelChains
                , players = M.adjust buyAndPayStock player players
                , turn = case turn of
                           (_, BuySomeStock n) | n > 1 -> (player, BuySomeStock (n-1))
                           _                           -> (nextPlayer game, PlaceTile)
                }
      else game

playerPass :: Game -> Game
playerPass game = game { turn = case turn game of
                                 (_, ResolveMerger (DisposeStock player buyer buyee price (_:next:pys)) cont)  ->
                                   (next, ResolveMerger (DisposeStock player buyer buyee price (next:pys)) cont)
                                 (_, ResolveMerger (DisposeStock _ _ _  _ [_]) cont)           ->
                                   cont
                                 _                                             -> (nextPlayer game, PlaceTile)
                       }

-- |
-- == Merging chains
--
-- When a player places a hotel adjacent to two (or more) chains, a merger takes place.
-- The chain with more member hotels takes over the other(s).  If the chains are the same size
-- before the merging hotel was placed, the person who made the merger chooses which chain takes over.
-- The chain marker of the now defunct chain is removed.  (See below, at Multiple Mergers,
-- for what to do when placing one hotel that merges more than one chain).
--
-- A chain containing 11 or more hotels is "safe" and cannot be taken over by another chain.
-- A player may not place a hotel which would merge two safe chains.  A safe chain may still take over an open chain, however.
twoChainsMerger :: ChainName -> ChainName -> Tile -> PlayerName -> Game -> Game
twoChainsMerger c1 c2 tile name game@Game{..} =
  if not (isSafe (hotelChains M.! c1)) || not (isSafe (hotelChains M.! c2))
  then game { turn = (name, ResolveMerger (TakeOver tile [c1,c2]) ((nextPlayer game), PlaceTile)) }
  else game

-- |
-- === Multiple mergers
--
-- When multiple mergers (mergers of more than two chains at once) occur, the larger chain takes over all smaller
-- chains simultaneously.  The multiple merger is then handled as individual mergers.  Majority holder’s bonuses
-- are paid for the larger of defunct chains and players dispose of their stock in this chain.  Then the smaller
-- defunct chain is handled in the same manner.  In the case of a tie, the merger maker chooses which defunct chain
-- is to be handled first.  Bonuses and stock prices are determined by the number of hotels in the defunct chain
-- before the merger.
threeChainsMerger :: [ChainName] -> Tile -> PlayerName -> Game -> Game
threeChainsMerger mergedChains tile name game@Game{..} =
  let mergedChainsBySize = sortBy (compare `on` (negate . length . chainTiles)) $ map (hotelChains M.!) mergedChains
      largestChain = chainName $ head mergedChainsBySize
      secondChain = chainName $ head $ tail mergedChainsBySize
      smallestChain = chainName $ head $ tail $ tail mergedChainsBySize
  in if any (not . isSafe) mergedChainsBySize
     then game { turn = (name,
                         ResolveMerger (TakeOver tile [largestChain,secondChain])
                          (name, ResolveMerger (TakeOver tile [largestChain,smallestChain]) (nextPlayer game, PlaceTile)))
               }
     else game

-- |
-- === Majority holder's bonus.
--
-- At merger time, bonuses are paid to the two largest shareholders in the defunct chain.
-- In the case of a tie for the title of largest shareholder, the first and second bonuses
-- are combined and divided equally between the tying shareholders.  In the case of a tie for
-- the title of second largest shareholder, the second bonus is divided equally between the
-- tying players.  If only one player holds stock in the defunct chain, he receives both bonuses.
computeMergerBonus :: Game -> ChainName -> Game
computeMergerBonus game@Game{..} chain =
  let buyeeChain = hotelChains M.! chain
      buyeeOwnedStock p = findOr0 chain (ownedStock p)
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
divideAmong plys amount game@Game{..} =
  let bonus = amount `div` length plys
      playersWithBonus = map (\ p -> p { ownedCash = bonus + ownedCash p}) plys
  in game { players = M.fromList (zip (map playerName playersWithBonus) playersWithBonus) `M.union` players }

-- |
-- === Disposal of stock at time of merger.
-- After the bonuses have been paid, each player, starting with the merger maker and continuing clockwise,
-- handles his/her stock in the defunct chain in one or more of the following ways:
--
--  1.  __Hold__:  Stock may be held in expectation of starting another chain with that name.
merge :: Game -> PlayerName -> Tile ->  ChainName -> ChainName -> Game
merge game@Game{..} _    tile buyer buyee =
  let buyerChain = hotelChains M.! buyer
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

-- |
--  2. __Sell__:  Stock may be sold to Stock Market at a price determined by the number of hotels in the defunct chain before the merger.
sellStock :: Game -> PlayerName -> ChainName -> Int -> Int -> Game
sellStock game@Game{..} player chain1 price qty =
  case stockLookup chain1 (ownedStock $ players M.! player) of
   Nothing -> game { turn = nextTurnInMergerSolving game turn }
   Just{}  -> let soldStock p = p { ownedStock = adjustStock (\ q -> q - qty) chain1 (ownedStock p)
                                  , ownedCash = ownedCash p + price * qty }
                  increaseStock c = c { chainStock = chainStock c + qty }
              in game { hotelChains = M.adjust increaseStock chain1 hotelChains
                      , players = M.adjust soldStock player players
                      , turn = nextTurnInMergerSolving game turn
                      }
-- |
--   3. __Trade__:  Stock may be traded at the rate of two blocks of defunct stock for one block of controlling chain stock.
-- (If the Stock Market has no remaining blocks of controlling chain stock, players may not trade).
exchangeStock :: Game  -> PlayerName -> ChainName -> ChainName -> Int -> Game
exchangeStock game@Game{..} player buyer buyee qty =
  case stockLookup buyee (ownedStock $ players M.! player) of
   Nothing -> game { turn = nextTurnInMergerSolving game turn }
   Just{}  -> let buyerRemainingStock = chainStock $ hotelChains M.! buyer
                  xchgedStock = min buyerRemainingStock qty
                  xchgStock p = p { ownedStock = adjustStock (+ xchgedStock) buyer $
                                                 adjustStock (\ k -> k - (xchgedStock * 2)) buyee (ownedStock p)
                                  }
                  increaseStock c = c { chainStock = chainStock c + (2 * xchgedStock) }
                  decreaseStock c = c { chainStock = chainStock c - xchgedStock }
              in game { hotelChains = M.adjust decreaseStock buyer $ M.adjust increaseStock buyee hotelChains
                      , players = M.adjust xchgStock player players
                      , turn = nextTurnInMergerSolving game turn
                      }

-- |
-- == Ending the game
-- The game ends when one player, during his turn, announces that either
-- all chains on board are "safe" or that any one chain contains 41 or more hotels.
-- A player does not have to announce that the game is over if it is to his advantage to continue playing.
-- After announcing that the game is over, the player may still complete his turn.
--
-- To determine the winner, all chains are bought out by the bank.  Majority holder’s
-- bonuses are paid for all active chains.  All players sell their stock.
-- Stock in a chain that is not on the board is worthless.  The player with the most money is the winner.
endGame :: Game -> Game
endGame game@Game{..} =
  let game' = foldl computeMergerBonus game (map chainName $ activeChains hotelChains)
      sellEverything p = p { ownedCash  = ownedCash p +
                                          (sum $ M.elems $ mapStock (\ k a -> stockPrice (hotelChains M.! k) * a) (ownedStock p))
                           , ownedStock = emptyStock
                                                 }
  in game' { players = M.map sellEverything players
           , turn = (fst turn, GameEnds) }
