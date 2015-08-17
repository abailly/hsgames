module Player where

import qualified Data.Map as M

import           Hotels
import           Tiles

data PlayerType = Human | Robot deriving (Eq, Show, Read)

data Player = Player { playerName :: String
                     , playerType :: PlayerType
                     , tiles      :: [ Tile ]
                     , ownedStock :: M.Map ChainName Int
                     , ownedCash  :: Int
                     } deriving (Eq, Show, Read)

type Players = M.Map PlayerName Player
type PlayerName = String

hasEnoughMoneyToBuyStock :: Player -> HotelChain -> Bool
hasEnoughMoneyToBuyStock player chain = let price = stockPrice chain
                                        in ownedCash player >= price

