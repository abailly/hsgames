module Bautzen.REPL

import Bautzen.Game
import Bautzen.REPL.SExpParser

import Data.Fin
import Prelude.Interactive

%default total

makeMoveCommand : (unitName : String) -> (col : Int) -> (row : Int) -> Either String (Command Move)
makeMoveCommand unitName col row with (fromIntegerNat (cast col), fromIntegerNat (cast row))
  | (c , r) with (isLTE c 22, isLTE r 12)
    | (Yes _, Yes _) = Right $ MoveTo unitName (Hex c r)
    | _              = Left $ "position should be between (0,0) and (22, 12): " ++ show col ++ ", " ++ show row


makeCommand : (game : Game) -> SExp -> Either String (Command  (curSegment game))
makeCommand game (SList [ SSym ":move", SStr unitName, SList [ SInt col, SInt row] ] ) with (curSegment game)
  | Move = makeMoveCommand unitName col row
  | other = Left $ "Invalid command for segment " ++ show other
makeCommand _ sexp = Left $ "Unknown command " ++ show sexp

partial
parseCommand : (game : Game) -> String -> Either String (Command (curSegment game))
parseCommand game input = do
  sexp <- parseSExp input
  makeCommand game sexp

partial
commandHandler : Game -> String -> (String, Game)
commandHandler game command =
  case parseCommand game command of
    Left err => (err, game)
    Right cmd => case act game cmd of
                      Left err => (show err, game)
                      Right event => (show event, apply event game)

partial
eoiHandler : Game -> String
eoiHandler = show

initialPositions : List (GameUnit, Pos)
initialPositions = [ (Bautzen.GameUnit.r13_5dp, Hex 3 4), (Bautzen.GameUnit.g21_20pz, Hex 3 5) ]

initialState : GameState
initialState = MkGameState 0 Axis Move initialPositions

initialGame : Game
initialGame = MkGame [] initialState

export
partial
repl : IO ()
repl = processStdin initialGame commandHandler eoiHandler
