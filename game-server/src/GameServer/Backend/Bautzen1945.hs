-- | Manages an instance of a backend server for /Bautzen 1945/ game.
module GameServer.Backend.Bautzen1945 where

import Control.Concurrent.Async (Async)
import Control.Exception (bracket)
import GameServer.Clients.IO (ServerConnection)

withBautzenServer ::
  ((ServerConnection IO, Async ()) -> IO a) ->
  IO a
withBautzenServer = bracket startBautzen stopBautzen

stopBautzen :: (ServerConnection IO, Async ()) -> IO ()
stopBautzen = error "not implemented"

startBautzen :: IO (ServerConnection IO, Async ())
startBautzen = error "not implemented"
