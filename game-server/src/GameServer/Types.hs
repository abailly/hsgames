module GameServer.Types where

import Control.Concurrent.Async
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.ByteString.Lazy (readFile)
import Data.Either (fromRight)
import GHC.Generics
import GameServer.Game (GameType)
import Prelude hiding (readFile)

data GameBackend = GameBackend
    { gameType :: GameType
    , gameHost :: String
    , gamePort :: Int
    , uiPath :: FilePath
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

selectBackend ::
    [GameBackend] -> GameType -> Maybe GameBackend
selectBackend [] _ = Nothing
selectBackend (b : bs) t
    | gameType b == t = Just b
    | otherwise = selectBackend bs t

data ServerConfiguration = ServerConfiguration
    { -- | The port this server should listen on. If 0, a random port
      --  will be chosen by the system
      serverPort :: Int
    , -- | List of `GameBackend` this server can connect to. If several
      --  backends are available for a given game, it will chose a random
      --  one
      backends :: [GameBackend]
    }

data Server = Server
    { -- | If the server is running, this will contain the underlying thread
      serverThread :: Maybe (Async ())
    , -- | The actual port this server is listening on. Useful when we start
      --  on a random port and need to know how to connect to the server, eg.
      --  for testing purposes
      serverPort :: Int
    }

readBackends :: Maybe FilePath -> IO [GameBackend]
readBackends Nothing = pure []
readBackends (Just backendFile) = fromRight [] . eitherDecode <$> readFile backendFile
