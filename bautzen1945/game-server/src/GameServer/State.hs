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

data Games = Games { seed :: StdGen
                   , games :: Map.Map GameId Game
                   }


initialState :: StdGen -> Games
initialState initSeed = Games initSeed mempty

type GameState = TVar Games

withState ::
  (MonadIO m) => GameState -> State Games a -> m a
withState st =
  liftIO . atomically . stateTVar st . runState

data Event = GameCreated { gameId :: GameId }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


-- * Commands

createGame :: Game -> State Games Event
createGame game = do
  gs <- get
  let gid = randomGameId (seed gs)
      (_,newSeed) = split (seed gs)
  put (gs { seed = newSeed, games = Map.insert gid game (games gs) })
  pure $ GameCreated gid
