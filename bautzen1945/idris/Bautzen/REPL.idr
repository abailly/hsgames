module Bautzen.REPL

import Debug.Trace
import Bautzen.Game
import Bautzen.Options
import Bautzen.REPL.SExpParser
import Bautzen.REPL.SExpInstances
import Bautzen.SExp

import System.REPL.Extra

import Data.Fin
import Data.Nat

data CmdREPL : (segment : GameSegment) -> Type where
  Cmd : (cmd : Command segment) -> CmdREPL segment
  Qry : (ToSExp res) => (qry : Query res) -> CmdREPL segment

makePos : (col : Int) -> (row : Int) -> Either String Pos
makePos col row with (integerToNat (cast col), integerToNat (cast row))
  makePos col row | (c , r) with (natToFin c 23, natToFin r 13)
    makePos col row | (c , r)  | (Just c', Just r') = Right $ hex c' r'
    makePos col row | (c , r)  | _              = Left $ "position should be between (0,0) and (22, 12): " ++ show col ++ ", " ++ show row

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

foo : (game : Game) -> String -> String
foo game s with (curSegment game)
  foo game s | Move = "Move"
  foo game s | _ = "other"

makeCommand : (game : Game) -> SExp -> Either String (CmdREPL (curSegment game))
makeCommand game (SList [ SSym "move!", SStr unitName, SList [ SInt col, SInt row] ] ) with (curSegment game)
  makeCommand game (SList [ SSym "move!", SStr unitName, SList [ SInt col, SInt row] ] ) | Move = Cmd <$> makeMoveCommand unitName col row
  makeCommand game (SList [ SSym "move!", SStr unitName, SList [ SInt col, SInt row] ] ) | other = Left ("Invalid command for segment " ++ show other)
makeCommand game (SList [ SSym "attack!", unitNames, SList [ SInt col, SInt row] ] ) with (curSegment game)
  makeCommand game (SList [ SSym "attack!", unitNames, SList [ SInt col, SInt row] ] ) | Combat NoCombat = Cmd <$> makeAttackWithCommand unitNames col row
  makeCommand game (SList [ SSym "attack!", unitNames, SList [ SInt col, SInt row] ] ) | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (SList [ SSym "support!", unitNames ] ) with (curSegment game)
  makeCommand game (SList [ SSym "support!", unitNames ] ) | Combat (AssignTacticalSupport side combatState) = Cmd <$> makeSupportCommand unitNames
  makeCommand game (SList [ SSym "support!", unitNames ] ) | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (SList [ SSym "resolve!" ] ) with (curSegment game)
  makeCommand game (SList [ SSym "resolve!" ] ) | Combat (Resolve combatState) = pure $ Cmd (ResolveCombat combatState)
  makeCommand game (SList [ SSym "resolve!" ] ) | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (SList [ SSym "lose-step!", SStr unitName ] ) with (curSegment game)
  makeCommand game (SList [ SSym "lose-step!", SStr unitName ] ) | Combat (ApplyLosses side state) = Cmd <$> makeLoseStepCommand unitName
  makeCommand game (SList [ SSym "lose-step!", SStr unitName ] ) | other = Left $ "Invalid command for segment " ++ show other
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
export
commandHandler : Game -> String -> (String, Game)
commandHandler game command =
  case parseCommand game command of
    Left err => (err, game)
    Right (Cmd cmd) => case act game cmd of
                         Left err => (show (toSExp err), game)
                         Right event => (show (toSExp event), apply event game)
    Right (Qry qry) =>
      let qryResult = toSExp $ query game qry
      in (show qryResult, game)


partial
eoiHandler : Game -> String
eoiHandler = show

initialPositions : List (GameUnit, Pos)
initialPositions = [ (Bautzen.GameUnit.p13_5dp, hex 1 9)
                   , (Bautzen.GameUnit.g21_20pz, hex 5 8)
                   ]

initialState : GameState
initialState = MkGameState 5 Axis Move initialPositions

export
initialGame : Game
initialGame = MkGame [] initialState FullGameMap

export
partial
repl : Options -> IO ()
repl _ = processStdin initialGame commandHandler eoiHandler
