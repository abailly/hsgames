module Bautzen.REPL

import Bautzen.Game
import Bautzen.REPL.SExpParser
import Bautzen.REPL.SExpInstances
import Bautzen.SExp

import Data.Fin
import Prelude.Interactive

%default total

data CmdREPL : (segment : GameSegment) -> Type where
  Cmd : (cmd : Command segment) -> CmdREPL segment
  Qry : (ToSExp res) => (qry : Query res) -> CmdREPL segment


makeMoveCommand : (unitName : String) -> (col : Int) -> (row : Int) -> Either String (Command Move)
makeMoveCommand unitName col row with (fromIntegerNat (cast col), fromIntegerNat (cast row))
  | (c , r) with (isLTE c 22, isLTE r 12)
    | (Yes _, Yes _) = Right $ MoveTo unitName (Hex c r)
    | _              = Left $ "position should be between (0,0) and (22, 12): " ++ show col ++ ", " ++ show row


makeCommand : (game : Game) -> SExp -> Either String (CmdREPL (curSegment game))
makeCommand game (SList [ SSym ":move", SStr unitName, SList [ SInt col, SInt row] ] ) with (curSegment game)
  | Move = Cmd <$> makeMoveCommand unitName col row
  | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (SList [ SSym "supply-path?", SStr unitName ] ) = Right $ Qry $ SupplyPath unitName
makeCommand _ sexp = Left $ "Unknown command " ++ show sexp

partial
parseCommand : (game : Game) -> String -> Either String (CmdREPL (curSegment game))
parseCommand game input = do
  sexp <- parseSExp input
  makeCommand game sexp

partial
commandHandler : Game -> String -> (String, Game)
commandHandler game command =
  case parseCommand game command of
    Left err => (err, game)
    Right (Cmd cmd) => case act game cmd of
                         Left err => (show (toSExp err) ++ "\n", game)
                         Right event => (show (toSExp event) ++ "\n", apply event game)
    Right (Qry qry) => (show (toSExp $ query game GameMap qry) ++ "\n", game)

partial
eoiHandler : Game -> String
eoiHandler = show

initialPositions : List (GameUnit, Pos)
initialPositions = [ (Bautzen.GameUnit.p13_5dp, Hex 3 4)
                   , (Bautzen.GameUnit.g21_20pz, Hex 3 5)
                   ]

initialState : GameState
initialState = MkGameState 0 Axis Move initialPositions

initialGame : Game
initialGame = MkGame [] initialState

export
partial
repl : IO ()
repl = processStdin initialGame commandHandler eoiHandler
