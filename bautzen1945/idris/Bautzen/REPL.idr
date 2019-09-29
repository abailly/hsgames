module Bautzen.REPL

import Bautzen.Game
import Bautzen.REPL.SExpParser
import Bautzen.REPL.SExpInstances
import Bautzen.SExp

import Data.Fin
import Prelude.Interactive

%default total
%flag C "-O3"

data CmdREPL : (segment : GameSegment) -> Type where
  Cmd : (cmd : Command segment) -> CmdREPL segment
  Qry : (ToSExp res) => (qry : Query res) -> CmdREPL segment

makePos : (col : Int) -> (row : Int) -> Either String Pos
makePos col row with (fromIntegerNat (cast col), fromIntegerNat (cast row))
  | (c , r) with (isLTE c 22, isLTE r 12)
    | (Yes _, Yes _) = Right $ Hex c r
    | _              = Left $ "position should be between (0,0) and (22, 12): " ++ show col ++ ", " ++ show row

makeMoveCommand : (unitName : String) -> (col : Int) -> (row : Int) -> Either String (Command Move)
makeMoveCommand unitName col row = MoveTo unitName <$> makePos col row

makeAttackWithCommand : (unitNames : SExp) -> (col : Int) -> (row : Int) -> Either String (Command $ Combat NoCombat)
makeAttackWithCommand unitNames col row = do
  pos <- makePos col row
  units <- toStrings unitNames
  pure $ AttackWith units pos

makeSupportCommand : (unitNames : SExp) -> {side : Side} -> {combatState : CombatState}
                   -> Either String (Command $ Combat (AssignTacticalSupport side combatState))
makeSupportCommand unitNames =
  toStrings unitNames >>= Right . TacticalSupport

makeLoseStepCommand : (unitName : String) -> {side : Side} -> {combatState : CombatState}
                   -> Either String (Command $ Combat (ApplyLosses side combatState))
makeLoseStepCommand unitName = pure $ LoseStep unitName

makeCommand : (game : Game) -> SExp -> Either String (CmdREPL (curSegment game))
makeCommand game (SList [ SSym "move!", SStr unitName, SList [ SInt col, SInt row] ] ) with (curSegment game)
  | Move = Cmd <$> makeMoveCommand unitName col row
  | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (SList [ SSym "attack!", unitNames, SList [ SInt col, SInt row] ] ) with (curSegment game)
  | Combat NoCombat = Cmd <$> makeAttackWithCommand unitNames col row
  | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (SList [ SSym "support!", unitNames ] ) with (curSegment game)
  | Combat (AssignTacticalSupport side combatState) = Cmd <$> makeSupportCommand unitNames
  | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (SList [ SSym "resolve!" ] ) with (curSegment game)
  | Combat (Resolve combatState) = pure $ Cmd (ResolveCombat combatState)
  | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (SList [ SSym "lose-step!", SStr unitName ] ) with (curSegment game)
  | Combat (ApplyLosses side state) = Cmd <$> makeLoseStepCommand unitName
  | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (SList [ SSym "next!" ] ) = pure $ Cmd NextSegment
makeCommand game (SList [ SSym "supply-path?", SStr unitName ] ) = Right $ Qry $ SupplyPath unitName
makeCommand game (SList [ SSym "map?" ] ) = Right $ Qry TerrainMap
makeCommand game (SList [ SSym "positions?" ] ) = Right $ Qry Positions
makeCommand game (SList [ SSym "stage?" ] ) = Right $ Qry GameStage
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
    Right (Qry qry) => (show (toSExp $ query game qry) ++ "\n", game)

partial
eoiHandler : Game -> String
eoiHandler = show

initialPositions : List (GameUnit, Pos)
initialPositions = [ (Bautzen.GameUnit.p13_5dp, Hex 1 9)
                   , (Bautzen.GameUnit.g21_20pz, Hex 5 8)
                   ]

initialState : GameState
initialState = MkGameState 5 Axis Move initialPositions

initialGame : Game
initialGame = MkGame [] initialState FullGameMap

export
partial
repl : IO ()
repl = processStdin initialGame commandHandler eoiHandler
