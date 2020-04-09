module GameServer.Player where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import GHC.Generics
import Servant.API (FromHttpApiData(..))

import GameServer.Utils

data Player = Player { playerName :: Text }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Unique key for player to access a game she joined
-- This key should be strictly kept private by the player as it
-- basically gives access to the game from the point of view of
-- that player
data PlayerKey = PlayerKey { playerKey :: Id }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromHttpApiData PlayerKey where
  parseUrlPiece = Right . PlayerKey . Id

data PlayerName = PlayerName { pName :: Text }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
