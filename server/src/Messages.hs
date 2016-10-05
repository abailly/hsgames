{-# LANGUAGE DeriveGeneric #-}

module Messages where

import           Data.Aeson
import           GHC.Generics

data Message = List
             | NewGame { numHumans :: Int, numRobots :: Int }
             | JoinGame { playerName :: String, gameId :: String }
             | Action { selectedPlay :: Int }
             | Bye
             deriving (Eq, Show, Read, Generic)

instance FromJSON Message
instance ToJSON Message
