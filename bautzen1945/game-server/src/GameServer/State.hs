module GameServer.State where

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans (MonadIO(..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Natural
import System.Random (StdGen, split)

import GameServer.Game
import GameServer.Player
import GameServer.Utils

data Games = Games { seed :: StdGen
                   , games :: Map.Map Id Game
                   , players :: Map.Map Text Player
                   , gamesByName :: Map.Map Text Game
                   }

initialState :: StdGen -> Games
initialState initSeed = Games initSeed mempty mempty mempty

type GameState = TVar Games

withState ::
  (MonadIO m) => GameState -> State Games a -> m a
withState st =
  liftIO . atomically . stateTVar st . runState

data Event = GameCreated { gameId :: Id }
           | DuplicateGame { duplicateName :: Text }
           | PlayerRegistered { registeredName :: Text }
           | DuplicatePlayer { duplicateName :: Text }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype GameError = GameError { reason :: Event }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- * Queries

listGames :: State Games [Game]
listGames = Map.elems <$> gets games

listPlayers :: State Games [Player]
listPlayers = Map.elems <$> gets players

-- * Commands

createGame :: Game -> State Games Event
createGame game = do
  gs <- get
  let gid = randomId (seed gs)
      (_,newSeed) = split (seed gs)
  case Map.lookup (gameName game) (gamesByName gs) of
    Just _ -> pure $ DuplicateGame (gameName game)
    Nothing -> do
      put (gs { seed = newSeed
              , games = Map.insert gid game (games gs)
              , gamesByName = Map.insert (gameName game) game (gamesByName gs) })
      pure $ GameCreated gid

registerPlayer :: Player -> State Games Event
registerPlayer player@Player{playerName} = do
  gs <- get
  case Map.lookup playerName (players gs) of
    Just _ -> pure $ DuplicatePlayer playerName
    Nothing -> do
      put (gs { players = Map.insert playerName player (players gs) })
      pure $ PlayerRegistered playerName
