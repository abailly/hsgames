module GameServer.Game where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import GHC.Generics
import System.Random (StdGen, randomRs)

data GameType = Bautzen1945 | Acquire
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Game = Game { gameType :: GameType
                 , gamePlayers :: [Text]
                 }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
