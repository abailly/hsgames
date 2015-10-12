{-# LANGUAGE RecordWildCards #-}
module Chinese.Player where

import qualified Data.Map as M


data Player = Player { playerName      :: PlayerName
                     , numberOfAnswers :: Int
                     , numberOfErrors  :: Int
                     } deriving (Show)

type Players = M.Map PlayerName Player
type PlayerName = String

newPlayer :: String -> Player
newPlayer name = Player name 0 0

successAnswer :: Player -> Player
successAnswer Player{..} = Player playerName (numberOfAnswers + 1) numberOfErrors

failureAnswer :: Player -> Player
failureAnswer Player{..} = Player playerName (numberOfAnswers + 1) (numberOfErrors + 1)

-- | Success rate
successRate :: Player -> Double
successRate Player{..} | numberOfAnswers == 0 = 0
                       | otherwise           = 1.0 - (fromIntegral numberOfErrors / fromIntegral numberOfAnswers)
