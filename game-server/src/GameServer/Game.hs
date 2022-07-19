module GameServer.Game where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import GHC.Generics
import GHC.Natural
import GameServer.Utils
import Servant.API (FromHttpApiData (..))
import System.Random (StdGen, randomRs)

data GameType = Bautzen1945 | Acquire
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FromHttpApiData GameType where
    parseUrlPiece "Bautzen1945" = Right Bautzen1945
    parseUrlPiece "Acquire" = Right Acquire
    parseUrlPiece other = Left $ "Unknown game type: " <> other

maxNumberOfPlayers :: GameType -> Natural
maxNumberOfPlayers Bautzen1945 = 2
maxNumberOfPlayers Acquire = 6

asText :: GameType -> Text
asText Bautzen1945 = "Bautzen1945"
asText Acquire = "Acquire"

data Game = Game
    { gameType :: GameType
    , numHumanPlayers :: Natural
    , numRobotPlayers :: Natural
    , gamePlayers :: Map.Map Id PlayerState
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

makeNewGame :: GameType -> Natural -> Game
makeNewGame gType numRobots = Game gType (maxNumberOfPlayers gType - numRobots) numRobots Map.empty

-- | The state of a `Player` who joined the game
data PlayerState = PlayerState
    { playerKey :: Id
    , playerName :: Text
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

hasJoinedGame :: Text -> Game -> Bool
hasJoinedGame pName Game{gamePlayers} = Map.filter (\ps -> playerName ps == pName) gamePlayers /= Map.empty

joinPlayer :: Text -> Id -> Game -> Game
joinPlayer pName pKey game@Game{gamePlayers} = game{gamePlayers = Map.insert pKey (PlayerState pKey pName) gamePlayers}

lookupPlayerState :: Id -> Game -> Maybe PlayerState
lookupPlayerState pKey Game{gamePlayers} = Map.lookup pKey gamePlayers

canStart :: Game -> Bool
canStart Game{..} = numHumanPlayers == fromIntegral (Map.size gamePlayers)
