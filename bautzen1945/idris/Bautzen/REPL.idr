module Bautzen.REPL

import Debug.Trace
import Bautzen.Game
import Bautzen.Options
import Bautzen.REPL.JSON

import System.REPL.Extra
import System.Concurrency

import Data.Fin
import Data.Nat
import Language.JSON

data CmdREPL : (seg : GameSegment) -> Type where
  Cmd : (cmd : Command seg) -> CmdREPL seg
  Qry : Cast res JSON => (qry : Query res) -> CmdREPL seg

makePos : (col : Int) -> (row : Int) -> Either String Pos
makePos col row with (integerToNat (cast col), integerToNat (cast row))
  makePos col row | (c , r) with (natToFin c 23, natToFin r 13)
    makePos col row | (c , r)  | (Just c', Just r') = Right $ hex c' r'
    makePos col row | (c , r)  | _              = Left $ "position should be between (0,0) and (22, 12): " ++ show col ++ ", " ++ show row

makeMoveCommand : (unitName : String) -> (col : Int) -> (row : Int) -> Either String (Command Move)
makeMoveCommand unitName col row = MoveTo unitName <$> makePos col row

makeAttackWithCommand : (unitNames : JSON) -> (col : Int) -> (row : Int) -> Either String (Command $ Combat NoCombat)
makeAttackWithCommand unitNames col row = do
  pos <- makePos col row
  units <- toStrings unitNames
  pure $ AttackWith units pos

makeSupportCommand : (unitNames : JSON) -> {side : Side} -> {combatState : CombatState}
                   -> Either String (Command $ Combat (AssignTacticalSupport side combatState))
makeSupportCommand unitNames =
  toStrings unitNames >>= Right . TacticalSupport

makeLoseStepCommand : (unitName : String) -> {side : Side} -> {combatState : CombatState}
                   -> Either String (Command $ Combat (ApplyLosses side combatState))
makeLoseStepCommand unitName = pure $ LoseStep unitName

makeCommand : (game : Game) -> JSON -> Either String (CmdREPL (curSegment game))
makeCommand game (JArray [ JString "move!", JString unitName, JArray [ JNumber col, JNumber row] ] ) with (curSegment game)
  makeCommand game (JArray [ JString "move!", JString unitName, JArray [ JNumber col, JNumber row] ] ) | Move = Cmd <$> makeMoveCommand unitName (cast col) (cast row)
  makeCommand game (JArray [ JString "move!", JString unitName, JArray [ JNumber col, JNumber row] ] ) | other = Left ("Invalid command for segment " ++ show other)
makeCommand game (JArray [ JString "attack!", unitNames, JArray [ JNumber col, JNumber row] ] ) with (curSegment game)
  makeCommand game (JArray [ JString "attack!", unitNames, JArray [ JNumber col, JNumber row] ] ) | Combat NoCombat = Cmd <$> makeAttackWithCommand unitNames (cast col) (cast row)
  makeCommand game (JArray [ JString "attack!", unitNames, JArray [ JNumber col, JNumber row] ] ) | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (JArray [ JString "support!", unitNames ] ) with (curSegment game)
  makeCommand game (JArray [ JString "support!", unitNames ] ) | Combat (AssignTacticalSupport side combatState) = Cmd <$> makeSupportCommand unitNames
  makeCommand game (JArray [ JString "support!", unitNames ] ) | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (JArray [ JString "resolve!" ] ) with (curSegment game)
  makeCommand game (JArray [ JString "resolve!" ] ) | Combat (Resolve combatState) = pure $ Cmd (ResolveCombat combatState)
  makeCommand game (JArray [ JString "resolve!" ] ) | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (JArray [ JString "lose-step!", JString unitName ] ) with (curSegment game)
  makeCommand game (JArray [ JString "lose-step!", JString unitName ] ) | Combat (ApplyLosses side state) = Cmd <$> makeLoseStepCommand unitName
  makeCommand game (JArray [ JString "lose-step!", JString unitName ] ) | other = Left $ "Invalid command for segment " ++ show other
makeCommand game (JArray [ JString "next!" ] ) = pure $ Cmd NextSegment
makeCommand game (JArray [ JString "supply-path?", JString unitName ] ) = Right $ Qry $ SupplyPath unitName
makeCommand game (JArray [ JString "map?" ] ) = Right $ Qry TerrainMap
makeCommand game (JArray [ JString "positions?" ] ) = Right $ Qry Positions
makeCommand game (JArray [ JString "stage?" ] ) = Right $ Qry GameStage
makeCommand _ sexp = Left $ "Unknown command " ++ show sexp

partial
parseCommand : (game : Game) -> String -> Either String (CmdREPL (curSegment game))
parseCommand game input = do
  sexp <- maybe (Left "cannot parse JSON") Right (parse input)
  makeCommand game sexp

partial
export
handleCommand : Game -> String -> (String, Game)
handleCommand game command =
  case parseCommand game command of
    Left err => (err, game)
    Right (Cmd cmd) => case act game cmd of
                         Left err => (show $ cast {to = JSON} err, game)
                         Right event => (show $ cast {to = JSON} event, apply event game)
    Right (Qry qry) =>
      let qryResult = cast {to=JSON} $ query game qry
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
commandLoop : Channel String -> Channel String -> String -> Game -> IO ()
commandLoop cin cout clientId game = do
   msg <- channelGet cin
   let (res, game') = handleCommand game msg
   channelPut cout res
   commandLoop cin cout clientId game'


export
partial
repl : Options -> IO ()
repl _ = processStdin initialGame handleCommand eoiHandler
