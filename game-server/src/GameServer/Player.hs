module GameServer.Player where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import GHC.Generics
import GameServer.Utils
import Servant.API (FromHttpApiData (..))

newtype Player = Player {playerName :: Text}
  deriving newtype (Eq, Show, ToJSON, FromJSON)

-- | Unique key for player to access a game she joined
-- This key should be strictly kept private by the player as it
-- basically gives access to the game from the point of view of
-- that player
newtype PlayerKey = PlayerKey {playerKey :: Id}
  deriving newtype (Eq, Show, ToJSON, FromJSON)

instance FromHttpApiData PlayerKey where
  parseUrlPiece = Right . PlayerKey . Id

newtype PlayerName = PlayerName {pName :: Text}
  deriving newtype (Eq, Show, ToJSON, FromJSON)
