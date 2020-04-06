{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module GameServer.App where

import Control.Concurrent.STM (TVar)
import Control.Monad.Trans (lift)
import GameServer.Log (LoggerEnv)
import Network.Wai (Application)
import Servant
import Servant.Server

data GameState

instance Semigroup GameState
instance Monoid GameState

type API = Raw

api :: Proxy API
api = Proxy

runApp :: LoggerEnv IO -> TVar GameState -> Application -> Application
runApp _logger _state statics = serve api (Tagged statics)
