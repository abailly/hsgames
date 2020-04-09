{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module GameServer.App
  ( module GameServer.Game
  , module GameServer.State
  , runApp
  ) where

import Control.Concurrent.STM (TVar)
import Control.Monad.Trans (lift)
import Data.Aeson (encode, FromJSON, ToJSON)
import Data.Text (Text)
import GameServer.Log (LoggerEnv)
import GHC.Generics
import Network.Wai (Application)
import Servant
import Servant.Server
import System.Random

import GameServer.Game
import GameServer.Player as P
import GameServer.Utils
import GameServer.State

type API = "games" :> ( Get '[JSON] [Game]
                      :<|> ReqBody '[JSON] Game :> PostCreated '[ JSON] (Headers '[Header "Location" Text] NoContent)
                      :<|> Capture "gameId" Id :> Get '[JSON] Game
                      :<|> Capture "gameId" Id :> "players" :> ReqBody '[JSON] PlayerName :> Put '[JSON] PlayerKey
                      :<|> Capture "gameId" Id :> "players" :> Capture "playerKey" PlayerKey :> Get '[JSON] (Headers '[Header "Location" Text] PlayerState)
                      )
           :<|> "players" :> ( Get '[JSON] [Player]
                               :<|> ReqBody '[JSON] Player :> PostCreated '[JSON] (Headers '[Header "Location" Text] NoContent)
                             )
           :<|> Raw

api :: Proxy API
api = Proxy

runApp :: LoggerEnv IO -> GameState -> Application -> Application
runApp _logger state statics = serve api ((listGamesH :<|> createGameH :<|> lookupGameH :<|> joinPlayerH :<|> startGameH) :<|> (listPlayersH :<|> registerPlayerH) :<|> Tagged statics)
  where
    listGamesH = withState state listGames

    createGameH game = do
      result <- withState state (createGame game)
      case result of
        GameCreated{gameId} -> pure $ addHeader ("/games" </> unId gameId) NoContent
        d -> throwError err400 { errBody = encode (GameError d) }

    lookupGameH gameId = do
      result <- withState state (lookupGame gameId)
      either (const $ throwError err404) pure result

    joinPlayerH gameId PlayerName{P.pName} = do
      result <- withState state (applyCommand (JoinGame gameId pName))
      case result of
        Right PlayerJoined{gameId,playerName,playerKey} -> pure $ PlayerKey playerKey
        Left err -> throwError err400 { errBody = encode err }

    startGameH gameId PlayerKey{playerKey} = do
      res <- withState state (canStartGame gameId playerKey)
      case res of
        Right (WaitingForPlayers _ pstate) -> pure $ addHeader ("/games" </> unId gameId </> "players" </> unId playerKey) pstate
        Left err -> throwError err400 { errBody = encode err }

    listPlayersH = withState state listPlayers

    registerPlayerH player = do
      result <- withState state (registerPlayer player)
      case result of
        PlayerRegistered{} -> pure $ addHeader ("/players" </> (P.playerName player)) NoContent
        d -> throwError err400 { errBody = encode (GameError d) }
