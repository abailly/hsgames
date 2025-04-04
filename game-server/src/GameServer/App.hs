{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module GameServer.App (
    module GameServer.Game,
    module GameServer.State,
    runApp,
) where

import Control.Concurrent.STM (TVar, atomically, stateTVar)
import Control.Monad.State (evalState, runState)
import Control.Monad.Trans (lift, liftIO)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import GameServer.App.HTML (userInterface)
import GameServer.Game
import GameServer.Log (LoggerEnv)
import GameServer.Player as P
import GameServer.State
import GameServer.Types (GameBackend (GameBackend), selectBackend, uiPath)
import GameServer.Utils
import Network.HTTP.Types (notImplemented501)
import Network.Wai (Application, responseLBS)
import Servant
import Servant.Server
import System.Random

type API =
    "games"
        :> ( Get '[JSON] [Game]
                :<|> ReqBody '[JSON] Game :> PostCreated '[JSON] (Headers '[Header "Location" Text] NoContent)
                :<|> Capture "gameId" Id :> Get '[JSON] Game
                :<|> Capture "gameId" Id :> "players" :> ReqBody '[JSON] PlayerName :> Put '[JSON] PlayerKey
                :<|> Capture "gameId" Id :> "players" :> Capture "playerKey" PlayerKey :> Get '[JSON] (Headers '[Header "Location" Text] PlayerState)
                :<|> Capture "gameType" GameType :> Capture "gameId" Id :> "players" :> Capture "playerKey" PlayerKey :> Raw
           )
        :<|> "players"
            :> ( Get '[JSON] [Player]
                    :<|> ReqBody '[JSON] Player :> PostCreated '[JSON] (Headers '[Header "Location" Text] NoContent)
               )
        :<|> Raw

api :: Proxy API
api = Proxy

runApp :: LoggerEnv IO -> GameState -> [GameBackend] -> Application -> Application
runApp _logger state backends statics =
    serve
        api
        ( ( listGamesH
                :<|> createGameH
                :<|> lookupGameH
                :<|> joinPlayerH
                :<|> startGameH
                :<|> gamePageRawH
          )
            :<|> (listPlayersH :<|> registerPlayerH)
            :<|> Tagged statics
        )
  where
    listGamesH = withState state listGames

    createGameH game = do
        result <- withState state (createGame game)
        case result of
            GameCreated{gameId} -> pure $ addHeader ("/games" </> unId gameId) NoContent
            d -> throwError err400{errBody = encode (GameError d)}

    lookupGameH gameId = do
        result <- withState state (lookupGame gameId)
        either (const $ throwError err404) pure result

    joinPlayerH gameId PlayerName{P.pName} = do
        withState state (applyCommand (JoinGame gameId pName))
            >>= \case
                Right PlayerJoined{gameId, playerName, playerKey} -> pure $ PlayerKey playerKey
                Right res -> throwError err500{errBody = encode ("Unexpected result " <> show res)}
                Left err -> throwError err400{errBody = encode err}

    startGameH gameId PlayerKey{playerKey} =
        withState state (canStartGame gameId playerKey) >>= \case
            Right (WaitingForPlayers _ pstate) ->
                pure $ addHeader ("/games" </> unId gameId </> "players" </> unId playerKey) pstate
            Right (CanStartPlaying game player) ->
                let redirect =
                        -- by convention this is a websocket address
                        encodeUtf8 $ "/games" </> asText (gameType game) </> unId gameId </> "players" </> unId playerKey
                 in throwError $ err303{errHeaders = [("Location", redirect)], errBody = encode player}
            Right res -> throwError err500{errBody = encode ("Unexpected result " <> show res)}
            Left err -> throwError err400{errBody = encode err}

    listPlayersH = withState state listPlayers

    registerPlayerH player =
        withState state (registerPlayer player) >>= \case
            PlayerRegistered{} -> pure $ addHeader ("/players" </> P.playerName player) NoContent
            d -> throwError err400{errBody = encode (GameError d)}

    gamePageRawH :: GameType -> Id -> PlayerKey -> Tagged Handler Application
    gamePageRawH gameType gameId PlayerKey{playerKey} =
        case selectBackend backends gameType of
            Just GameBackend{uiPath} -> Tagged $ userInterface uiPath
            Nothing -> Tagged statics
