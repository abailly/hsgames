module GameServer.Player where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import GHC.Generics

data Player = Player { playerName :: Text }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
