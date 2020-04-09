module GameServer.Types where

import           Control.Concurrent.Async

data Server = Server
    { serverThread :: Maybe (Async ())
    -- ^If the server is running, this will contain the underlying thread
    , serverPort   :: Int
    -- ^The actual port this server is listening on. Useful when we start
    -- on a random port and need to know how to connect to the server, eg.
    -- for testing purposes
    }
