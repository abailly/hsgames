{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module GameServer.App where

import Control.Concurrent.STM (TVar)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, ToJSON)
import GameServer.Log (LoggerEnv)
import GHC.Generics
import Network.Wai (Application)
import Servant
import Servant.Server

data GameState

instance Semigroup GameState
instance Monoid GameState

data Game = Game
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type API = "games" :> Get '[JSON] [Game]
           :<|> Raw

api :: Proxy API
api = Proxy

runApp :: LoggerEnv IO -> TVar GameState -> Application -> Application
runApp _logger _state statics = serve api (listGames :<|> Tagged statics)
  where
    listGames = pure []
