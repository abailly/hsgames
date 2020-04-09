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
                   }

initialState :: StdGen -> Games
initialState initSeed = Games initSeed mempty mempty

type GameState = TVar Games

withState ::
  (MonadIO m) => GameState -> State Games a -> m a
withState st =
  liftIO . atomically . stateTVar st . runState

data Command = JoinGame { gameId :: Id, pName :: Text }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Event = GameCreated { gameId :: Id }
           | PlayerJoined { gameId :: Id, playerName :: Text, playerKey :: Id }
           | PlayerAlreadyJoinedGame { gameId :: Id, playerName :: Text }
           | PlayerRegistered { registeredName :: Text }
           | DuplicatePlayer { duplicateName :: Text }
           | PlayerDoesNotExist { playerName :: Text }
           | PlayerNotInGame { gameId :: Id, pKey :: Id }
           | UnknownGame { gameId :: Id }
           | CanStartPlaying { game :: Game, player :: PlayerState }
           | WaitingForPlayers { gameId :: Id, pState :: PlayerState }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype GameError = GameError { reason :: Event }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

-- * Queries

listGames :: State Games [Game]
listGames = Map.elems <$> gets games

lookupGame :: Id -> State Games (Either GameError Game)
lookupGame gameId  = maybe (Left $ GameError $ UnknownGame gameId) Right . Map.lookup gameId <$> gets games

listPlayers :: State Games [Player]
listPlayers = Map.elems <$> gets players

lookupPlayer :: Text -> State Games (Either GameError Player)
lookupPlayer pName = maybe (Left $ GameError $ PlayerDoesNotExist pName) Right . Map.lookup pName  <$> gets players


-- * Commands

applyCommand :: Command -> State Games (Either GameError Event)
applyCommand JoinGame{gameId,pName} = runExceptT $ do
  game <- ExceptT $ lookupGame gameId
  when (hasJoinedGame pName game) $ throwError $ GameError $ PlayerAlreadyJoinedGame gameId pName
  player <- ExceptT $ lookupPlayer pName
  pkey <- lift mkRandomId
  let game' = joinPlayer pName pkey game
  modify' $ \ gs -> gs { games = Map.insert gameId game' (games gs) }
  pure $ PlayerJoined gameId pName pkey

createGame :: Game -> State Games Event
createGame game = do
  gs <- get
  let gid = randomId (seed gs)
      (_,newSeed) = split (seed gs)
  put (gs { seed = newSeed
          , games = Map.insert gid game (games gs)
          })
  pure $ GameCreated gid

registerPlayer :: Player -> State Games Event
registerPlayer player@Player{playerName} = do
  gs <- get
  case Map.lookup playerName (players gs) of
    Just _ -> pure $ DuplicatePlayer playerName
    Nothing -> do
      put (gs { players = Map.insert playerName player (players gs) })
      pure $ PlayerRegistered playerName

canStartGame :: Id -> Id -> State Games (Either GameError Event)
canStartGame gameId playerKey = runExceptT $ do
  game <- ExceptT $ lookupGame gameId
  case lookupPlayerState playerKey game of
    Nothing -> throwError $ GameError $ PlayerNotInGame gameId playerKey
    Just player -> if canStart game
                   then pure $ CanStartPlaying game player
                   else pure $ WaitingForPlayers gameId player

-- * Generic State functions

-- | Generate a new random Id from the current `Games` state seed
-- this function also updates the current seed
mkRandomId :: State Games Id
mkRandomId = do
  gs <- get
  let gid = randomId (seed gs)
      (_,newSeed) = split (seed gs)
  put gs { seed = newSeed}
  pure gid
