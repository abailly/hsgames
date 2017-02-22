{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
module Acquire.Game.Hotels where

import           Acquire.Game.Tiles
import           Data.Aeson         (FromJSON, ToJSON (..), ToJSONKey)
import qualified Data.Map           as M
import           GHC.Generics

data ChainName = American | Continental | Festival | Imperial | Luxor | Tower | Worldwide
                deriving (Eq, Enum, Ord, Show, Read, Generic)

instance ToJSON ChainName
instance ToJSONKey ChainName
instance FromJSON ChainName

data HotelChain = HotelChain { chainName  :: ChainName
                             , chainTiles :: [ Tile ]
                             , chainStock :: Int
                             } deriving (Eq, Show, Read, Generic)

instance ToJSON HotelChain
instance FromJSON HotelChain

type HotelChains = M.Map ChainName HotelChain

isActive :: HotelChain -> Bool
isActive HotelChain{..} = not (null chainTiles)

isSafe :: HotelChain -> Bool
isSafe HotelChain{..} = length chainTiles >= 11

isOverLimit :: HotelChain -> Bool
isOverLimit HotelChain{..} = length chainTiles >= 41

gameCanEnd :: HotelChains -> Bool
gameCanEnd chains = (not $ null $ activeChains chains) &&
                    (all isSafe (activeChains chains)     ||
                      any isOverLimit (activeChains chains))

activeChains :: HotelChains -> [ HotelChain ]
activeChains chains = M.elems $ M.filter isActive chains


maximumStock :: Int
maximumStock = 25

stockPrice :: HotelChain -> Int
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

mergerBonus :: HotelChain -> (Int, Int)
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
