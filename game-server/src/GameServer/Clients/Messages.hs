module GameServer.Clients.Messages where

import Data.Aeson
import Data.Text (Text)
import GameServer.Utils
import GHC.Generics
import GHC.Natural

data Message =
  List
  | NewGame { numHumans :: Natural, numRobots :: Natural }
  | JoinGame { playerKey :: Id, gameId :: Id }
  | Action { selectedPlay :: Natural }
  | Bye
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass( ToJSON, FromJSON)

newtype CommandError = CommandError { reason :: Text }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data Result =
  PlayerRegistered { playerKey :: Id, gameId :: Id }
  | NewGameStarted { gameId :: Id }
  | GameStarts { gameId :: Id }
  | GamesList { games :: [Id] }
  | ErrorMessage { error :: Text }
  deriving (Show, Read, Generic, ToJSON, FromJSON)

data GameDescription =
  GameDescription
  { gameDescId           :: Id
  , descNumberOfHumans   :: Natural
  , descNumberOfRobots   :: Natural
  , descRegisteredHumans :: [Id]
  , descLive             :: Bool
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
