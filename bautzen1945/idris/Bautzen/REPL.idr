module Bautzen.REPL

import Debug.Trace
import Bautzen.Games
import Bautzen.Options

import System.REPL.Extra

import Data.Fin
import Data.Nat
import Language.JSON


partial
parseCommand : (games : Games) -> String -> Either String GameCommand
parseCommand game input =
  maybe (Left $ "cannot parse JSON: " ++ input) Right (parse input) >>= makeGameCommand game

partial
export
handleCommand : Games -> String -> (String, Games)
handleCommand games input =
  case parseCommand games input of
    Left err => (err, games)
    Right act =>
        let res = interpret act games
            games' = apply games res
        in (show $ cast {to = JSON} res, games')

partial
eoiHandler : Games -> String
eoiHandler = show . cast {to = JSON}

export
partial
repl : Options -> IO ()
repl _ = processStdin initialGames handleCommand eoiHandler
