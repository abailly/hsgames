module GameServer.Types where

import Control.Concurrent.Async
import GameServer.Game (GameType)
import Network.Socket (PortNumber)

data GameBackend =
  GameBackend { gameType :: GameType
              , gameHost :: String
              , gamePort :: Int
              }

selectBackend ::
  [ GameBackend ] -> GameType -> Maybe GameBackend
selectBackend [] _ = Nothing
selectBackend (b:bs) t
  | gameType b == t  = Just b
  | otherwise   = selectBackend bs t

data ServerConfiguration =
  ServerConfiguration { serverPort :: Int
                        -- ^The port this server should listen on. If 0, a random port
                        -- will be chosen by the system
                      , backends :: [ GameBackend ]
                        -- ^List of `GameBackend` this server can connect to. If several
                        -- backends are available for a given game, it will chose a random
                        -- one
                      }

data Server = Server
    { serverThread :: Maybe (Async ())
    -- ^If the server is running, this will contain the underlying thread
    , serverPort   :: Int
    -- ^The actual port this server is listening on. Useful when we start
    -- on a random port and need to know how to connect to the server, eg.
    -- for testing purposes
    }
