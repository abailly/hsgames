{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module GameServer.App
  ( module GameServer.Game
  , initialState, runApp
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
import GameServer.Player
import GameServer.Utils
import GameServer.State

type API = "games" :> ( Get '[JSON] [Game]
                      :<|> ReqBody '[JSON] Game :> PostCreated '[ JSON] (Headers '[Header "Location" Text] NoContent)
                      )
           :<|> "players" :> ( Get '[JSON] [Player]
                               :<|> ReqBody '[JSON] Player :> PostCreated '[ JSON] (Headers '[Header "Location" Text] NoContent)
                             )
           :<|> Raw

api :: Proxy API
api = Proxy

runApp :: LoggerEnv IO -> GameState -> Application -> Application
runApp _logger state statics = serve api ((listGamesH :<|> createGameH) :<|> (listPlayersH :<|> registerPlayerH) :<|> Tagged statics)
  where
    listGamesH = withState state listGames

    createGameH game = do
      result <- withState state (createGame game)
      case result of
        GameCreated{gameId} -> pure $ addHeader ("/games" </> unId gameId) NoContent
        d -> throwError err400 { errBody = encode (GameError d) }

    listPlayersH = withState state listPlayers

    registerPlayerH player = do
      result <- withState state (registerPlayer player)
      case result of
        PlayerRegistered{} -> pure $ addHeader ("/players" </> (playerName player)) NoContent
        d -> throwError err400 { errBody = encode (GameError d) }
