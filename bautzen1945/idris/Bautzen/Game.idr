module Bautzen.Game

import public Bautzen.Combats
import public Bautzen.GameUnit
import public Bautzen.Game.Combat
import public Bautzen.Game.Core
import public Bautzen.Game.Move
import public Bautzen.Game.Supply
import public Bautzen.Game.Turn
import public Bautzen.Pos
import public Bautzen.Terrain
import public Bautzen.Game.Map

import Bautzen.SExp

import Language.JSON
import Data.List
import Data.Fin
import Data.Nat

export
act : (game : Game) -> Command (curSegment game) -> Either GameError Event
act (MkGame _ (MkGameState _ side Move units) gameMap) (MoveTo unitName to) = moveTo side units gameMap unitName to
act (MkGame _ (MkGameState _ side (Combat NoCombat) units) gameMap) (AttackWith unitNames target) = attackWith side units gameMap unitNames target
act game NextSegment = nextSegment game
act (MkGame _ (MkGameState _ side (Combat (AssignTacticalSupport combatSide combat)) units) gameMap) (TacticalSupport unitNames) =
  supportWith side combatSide units gameMap unitNames combat
act (MkGame _ (MkGameState _ side (Combat (Resolve combat)) units) gameMap) (ResolveCombat combat) =
  resolveCombat side combat
act (MkGame _ (MkGameState _ side (Combat (ApplyLosses lossSide combat)) units) gameMap) (LoseStep unitName) =
  loseStep side lossSide combat unitName

applyTacticalSupportEvent : Side -> CombatState -> List (GameUnit, Pos) -> GameState -> GameState
applyTacticalSupportEvent supportedSide (MkCombatState combatHex attackers defenders losses) units game =
  if side game == supportedSide
  then { segment := Combat (AssignTacticalSupport (flipSide supportedSide)
                                       (MkCombatState combatHex
                                        ({ tacticalSupport := units } attackers)
                                        defenders
                                        losses)) } game
  else { segment := Combat (AssignStrategicSupport (flipSide supportedSide)
                                       (MkCombatState combatHex
                                        attackers
                                        ({ tacticalSupport := units } defenders)
                                        losses)) } game

applySupplyColumnUsedEvent :  Side -> CombatState -> Pos -> GameState -> GameState
applySupplyColumnUsedEvent supportedSide (MkCombatState combatHex attackers defenders losses) hex game =
  if side game == supportedSide
  then { segment := Combat (AssignStrategicSupport supportedSide
                                       (MkCombatState combatHex
                                        ({ strategicSupport $= (+1) } attackers)
                                        defenders
                                        losses)) } game
  else { segment := Combat (AssignStrategicSupport supportedSide
                                       (MkCombatState combatHex
                                        attackers
                                        ({ strategicSupport $= (+1) } defenders)
                                        losses)) } game

-- TODO handle case where there are more losses to applies than units
applyStepLostEvent :
  Side -> GameUnit -> Losses -> CombatState -> GameState -> GameState
applyStepLostEvent lossSide unit newLosses state game@(MkGameState turn side segment units) =
  { segment := newSegment, units := reduced } game
  where
    reduced : List (GameUnit, Pos)
    reduced = reduce unit units

    newState : CombatState
    newState = { losses := Just newLosses } state

    newSegment : GameSegment
    newSegment = case newLosses of
                  (Z /> Z) => Combat NoCombat -- combat is completely resolved
                  (Z /> def) => Combat $ ApplyLosses (flipSide side) newState
                  (atk /> _) => Combat $ ApplyLosses side newState


applyEvent : Event -> GameState -> GameState
applyEvent (Moved unit from to cost) (MkGameState turn side segment units) =
  MkGameState turn side segment (updateMovedUnit unit to (Terrain.toNat cost) units)
applyEvent (CombatEngaged atk def tgt) game =
  { segment := Combat (AssignTacticalSupport (side game)
                     (MkCombatState tgt
                       (MkEngagedUnits atk [] 0)
                       (MkEngagedUnits def [] 0)
                       Nothing)) } game
applyEvent (TacticalSupportProvided _ units) game =
  case segment game of
    (Combat (AssignTacticalSupport supSide combat)) => applyTacticalSupportEvent supSide combat units game
    _ => game -- TODO make this impossible to happen
applyEvent (SupplyColumnUsed side pos) game =
  case segment game of
    (Combat (AssignStrategicSupport supSide combat)) => applySupplyColumnUsedEvent supSide combat pos game
    _ => game -- TODO make this impossible to happen
applyEvent (CombatResolved state losses) game =
  case segment game of
    (Combat (Resolve _)) => { segment := (Combat $ ApplyLosses (side game) ({ losses := Just losses } state)) } game
    _ => game -- TODO make this impossible to happen
applyEvent (StepLost unitSide unit newLosses) game =
  case segment game of
    (Combat (ApplyLosses lossSide combatState)) => applyStepLostEvent unitSide unit newLosses combatState game
    _ => game -- TODO make this impossible to happen
applyEvent (SegmentChanged from to) game =
  { segment := to } game
applyEvent AxisTurnDone game =
  { side := Allies, segment := Supply } game
applyEvent (TurnEnded n) game =
  { turn := n, side := Axis, segment := Supply } game
applyEvent GameEnded game =
  { segment := GameEnd } game

export
apply : Event -> Game -> Game
apply event (MkGame events curState gameMap) =
  MkGame (event :: events) (applyEvent event curState) gameMap


-- Queries

||| Query interface to retrieve information from a `Game`.
|||
||| Queries do not change the state of the game  but provide various
||| information, raw or computed, on its current state, in ways that
||| can be consumed by clients.
|||
||| @result the type of result produced by this instance of `Query`
public export
data Query : (result : Type) -> Type where

  ||| What's the (shortest) supply path for the given `unitName`?
  |||
  ||| Returns a list of `Pos`itions from a `SupplySource` hex to the current
  ||| position of the given unit, if there is one. Returns a `QueryError` if no
  ||| supply path can be drawn for this unit, either because there is none
  ||| or because the unit does not exist.
  ||| @unitName full name of unit to check supply for
  SupplyPath : (unitName : String) -> Query (Either QueryError (List Pos))

  ||| Retrieve the game's `Map`
  TerrainMap : Query Map

  ||| Retrieve positions of all units
  Positions : Query (List (GameUnit, Pos))

  ||| Retrieve the stage (turn, side, segment) the game is currently at
  GameStage : Query (Fin 6, Side, GameSegment)

export
Show (Query a) where
  show (SupplyPath u) = "SupplyPath " ++ show u
  show TerrainMap = "TerrainMap"
  show Positions = "Positions"
  show GameStage = "GameStage"

export
query : (Cast result JSON) => (game : Game) -> (qry : Query result) -> result
query (MkGame _ (MkGameState _ side _ units) gameMap) (SupplyPath unitName) =
  case find (sameName unitName) units of
    Nothing => Left (UnitDoesNotExist unitName)
    (Just (unit, pos)) =>
      case supplyPathTo units gameMap (supplySources (nation unit) gameMap) (unit, pos) of
        [] => Left (NoSupplyPathFor unitName pos)
        x  => Right x
query (MkGame _ (MkGameState _ side _ units) gameMap) TerrainMap = gameMap
query (MkGame _ (MkGameState _ side _ positions) _) Positions = positions
query (MkGame _ (MkGameState turn side segment _) _) GameStage = (turn, side, segment)
