module GameServer.Game where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import GHC.Generics
import System.Random (StdGen, randomRs)

data Game = Game { gameName :: Text
                 , gamePlayers :: [Text]
                 }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
