{-# LANGUAGE ViewPatterns #-}
module Acquire.Player where

import           Acquire.Hotels
import           Acquire.Tiles
import qualified Data.Map       as M

data PlayerType = Human | Robot deriving (Eq, Show, Read)

data Player = Player { playerName :: PlayerName
                     , playerType :: PlayerType
                     , tiles      :: [ Tile ]
                     , ownedStock :: M.Map ChainName Int
                     , ownedCash  :: Int
                     } deriving (Eq, Show, Read)

type Players = M.Map PlayerName Player
type PlayerName = String

isHuman :: Player -> Bool
isHuman (playerType -> Human) = True
isHuman _                    = False

isRobot :: Player -> Bool
isRobot (playerType -> Robot) = True
isRobot _                    = False

hasEnoughMoneyToBuyStock :: Player -> HotelChain -> Bool
hasEnoughMoneyToBuyStock player chain = let price = stockPrice chain
                                        in ownedCash player >= price

