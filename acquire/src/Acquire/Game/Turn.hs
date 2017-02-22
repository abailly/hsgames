{-# LANGUAGE RecordWildCards #-}
module Acquire.Game.Turn where

import qualified Data.Map            as M

import           Acquire.Game.Core
import           Acquire.Game.Hotels
import           Acquire.Game.Player

completeWithEndGame :: HotelChains -> [Order] -> [Order]
completeWithEndGame chains orders = if gameCanEnd chains
                                    then EndGame : orders
                                    else orders

possiblePlay :: Game -> [ Order ]
possiblePlay (Game _ _ _  _ _ (_, GameEnds))                      =  [Cancel]
possiblePlay (Game _ _ _ [] _ (_, _))                             =  [EndGame]
possiblePlay (Game _ _ plys _ chains (name, PlaceTile))           =
  completeWithEndGame chains $ map (Place name) (tiles $ plys M.! name)
possiblePlay (Game _ _ _ _ chains (name, ResolveMerger (TakeOver tile [c1,c2]) _))
  | length (chainTiles (chains M.! c1)) > length (chainTiles (chains M.! c2))
  = [Merge name tile c1 c2]
  | length (chainTiles (chains M.! c1)) < length (chainTiles (chains M.! c2))
  = [Merge name tile c2 c1]
  | otherwise
  = [Merge name tile c2 c1, Merge name tile c1 c2]
possiblePlay (Game _ _ plys _ _ (_, ResolveMerger (DisposeStock _ buyer buyee price (next:_)) _))
  =  case stockLookup buyee (ownedStock $ plys M.! next) of
       Nothing -> [Pass]
       Just n  ->
         Pass :
         [SellStock next buyee price k | k <- [ 1 .. n ] ] ++
         [ExchangeStock next buyer buyee k | k <- [ 1 .. n `div` 2 ] ]
possiblePlay game@(Game _ _ _ _ chains (name, FundChain t))
  =  let availableChainsForFounding = (filter (not . hasActiveChain game) $ M.keys chains)
     in if not (null availableChainsForFounding)
        then completeWithEndGame chains $
             map (\ c -> Fund name c t) availableChainsForFounding
        else [Pass]

possiblePlay game@(Game _ _ plys _ chains (name, BuySomeStock _)) =
  completeWithEndGame chains $
  Pass : (map (\ c -> BuyStock name c)                 $
           filter (\ c -> hasEnoughMoneyToBuyStock (plys M.! name) (chains M.! c)) $
           filter (hasActiveChain game)                $
           M.keys chains)
possiblePlay _ = [Cancel]

nextTurnInMergerSolving :: Game -> Turn -> Turn
nextTurnInMergerSolving game (_, ResolveMerger (DisposeStock player buyer buyee price (this:next:pys)) cont) =
  case stockLookup buyee (ownedStock $ (players game) M.! next) of
   Nothing -> (next, ResolveMerger (DisposeStock player buyer buyee price (next:pys)) cont)
   Just 0  -> (next, ResolveMerger (DisposeStock player buyer buyee price (next:pys)) cont)
   Just _  -> (this, ResolveMerger (DisposeStock player buyer buyee price (this:next:pys)) cont)
nextTurnInMergerSolving game (_, ResolveMerger (DisposeStock player buyer buyee price [this]) cont) =
  case stockLookup buyee (ownedStock $ (players game) M.! this) of
   Nothing -> cont
   Just 0  -> cont
   Just _  -> (this, ResolveMerger (DisposeStock player buyer buyee price [this]) cont)
nextTurnInMergerSolving _ _ = undefined

buyStockOrNextPlayer :: PlayerName -> Game -> (PlayerName, Phase)
buyStockOrNextPlayer name game@Game{..} = if   any (hasActiveChain game) (M.keys hotelChains)
                                          then (name, BuySomeStock 3)
                                          else (nextPlayer game, PlaceTile)


