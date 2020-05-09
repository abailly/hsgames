module Main where

import GameServer
import GameServer.Options
import GameServer.Types
import GameServer.Log

main :: IO ()
main = do
  logger <- newLog "game-server"
  Options{..} <- parseOptions
  backends <- readBackends backendDescriptor
  let config = ServerConfiguration port backends
  startServer logger config >>= waitServer
