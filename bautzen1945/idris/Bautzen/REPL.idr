module Bautzen.REPL

import Debug.Trace
import Bautzen.Games
import Bautzen.Id
import Bautzen.Options

import System.REPL.Extra

import Data.Fin
import Data.Nat
import JSON.Parser
import JSON

%hide JSON.Option.Options

export
parseCommand : (games : Games) -> String -> Either String GameCommand
parseCommand game input =
  either (Left . show) Right (parseJSON Virtual input) >>= makeGameCommand game

export
handleCommand : Id -> Games -> String -> (String, Games)
handleCommand clientId games input =
  case parseCommand games input of
    Left err => (err, games)
    Right act =>
       let (res, g) = interpret clientId act games
       in (show {ty = JSON} $ toJSON res, g)

eoiHandler : Games -> String
eoiHandler = show {ty = JSON} . toJSON

export
partial
repl : Options -> IO ()
repl opts = processStdin initialGames (handleCommand opts.instanceId) eoiHandler
