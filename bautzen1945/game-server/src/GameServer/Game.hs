module GameServer.Game where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import GHC.Generics
import System.Random (StdGen, randomRs)

data Game = Game { gameName :: Text }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype GameId = GameId { ungameId :: Text }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

randomGameId :: StdGen -> GameId
randomGameId = GameId . pack . take 8 . randomRs ('A','Z')
