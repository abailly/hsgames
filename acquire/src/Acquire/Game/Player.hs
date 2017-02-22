{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Acquire.Game.Player where

import           Acquire.Game.Hotels
import           Acquire.Game.Tiles
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (..),
                                      object, (.:), (.=))
import qualified Data.Map            as M
import           GHC.Generics


data PlayerType = Human | Robot deriving (Eq, Show, Read, Generic)

instance ToJSON PlayerType
instance FromJSON PlayerType

newtype Stock = Stock { stock :: M.Map ChainName Int }
  deriving (Eq, Show, Read)

emptyStock :: Stock
emptyStock = Stock M.empty

stockLookup :: ChainName -> Stock -> Maybe Int
stockLookup chain (Stock s) = M.lookup chain s

alterStock :: (Maybe Int -> Maybe Int) -> ChainName -> Stock -> Stock
alterStock f chain (Stock s) = Stock $ M.alter f chain s

adjustStock :: (Int -> Int) -> ChainName -> Stock -> Stock
adjustStock f chain (Stock s) = Stock $ M.adjust f chain s

mapStock :: (ChainName -> Int -> a) -> Stock -> M.Map ChainName a
mapStock f (Stock s) = M.mapWithKey f s

listStock :: Stock -> [(ChainName, Int)]
listStock (Stock s) = M.toList s

findOr0 :: ChainName -> Stock -> Int
findOr0 chain (Stock s) = M.findWithDefault 0 chain s

instance ToJSON Stock where
  toJSON (Stock s) = object [ "stock" .= M.toList s ]

instance FromJSON Stock where
  parseJSON (Object o) = Stock <$>
                         (M.fromList <$> o .: "stock")
  parseJSON v         = fail $ "Cannot parse Stock from JSON " ++ show v

data Player = Player { playerName :: PlayerName
                     , playerType :: PlayerType
                     , tiles      :: [ Tile ]
                     , ownedStock :: Stock
                     , ownedCash  :: Int
                     } deriving (Eq, Show, Read, Generic)

instance ToJSON Player
instance FromJSON Player

type Players = M.Map PlayerName Player
type PlayerName = String

isHuman :: Player -> Bool
isHuman (playerType -> Human) = True
isHuman _                     = False

isRobot :: Player -> Bool
isRobot (playerType -> Robot) = True
isRobot _                     = False

hasEnoughMoneyToBuyStock :: Player -> HotelChain -> Bool
hasEnoughMoneyToBuyStock player chain = let price = stockPrice chain
                                        in ownedCash player >= price

