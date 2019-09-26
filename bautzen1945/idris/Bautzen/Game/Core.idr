module Bautzen.Game.Core

import Bautzen.GameUnit
import Bautzen.Pos
import Bautzen.Terrain

import Data.Fin

%access public export
%default total

||| Combat result as steps lost by attacker and defender.
record Losses where
  constructor (/>)

  ||| Steps lost by attacker
  attackerLoss : Nat

  ||| Steps lost by defender. Can be transformed in hexes of retreat.
  defenderLoss : Nat

infix 1 />


record CombatState where
  constructor MkCombatState
  attackers : List (GameUnit, Pos)
  attackerSupport : Maybe Nat
  defenders : List (GameUnit, Pos)
  defenderSupport : Maybe Nat
  losses : Maybe Losses

||| * see section 8.2
data CombatPhase : Type where
  NoCombat : CombatPhase
  AssignTacticalSupport : (side : Side) -> (combat : CombatState) -> CombatPhase
  AssignStrategicSupport : (side : Side) -> (combat : CombatState) -> CombatPhase
  ApplyLosses : (side : Side) -> (losses : Nat) -> (combat : CombatState) -> CombatPhase

data GameSegment : Type where
  Supply : GameSegment
  Move : GameSegment
  Combat : (phase : CombatPhase) -> GameSegment
  GameEnd : GameSegment

Show GameSegment where
  show Supply = "Supply"
  show Move = "Move"
  show Combat = "Combat"
  show GameEnd = "GameEnd"

record GameState where
  constructor MkGameState
  turn : Fin 6
  side : Side
  segment : GameSegment
  units : List (GameUnit, Pos)

Show GameState where
  show (MkGameState turn side segment units) =
    "GameState: turn=" ++
    show (finToInteger turn) ++
    ", side=" ++ show side ++
    ", " ++ show segment ++
    ", units=" ++ show units

data GameError : Type where
  NoSuchUnits : (unitName : List String) -> GameError
  NotYourTurn : (side : Side) -> GameError
  EnemyInHex : (unit : GameUnit) -> (hex : Pos) -> GameError
  MoveFromZocToZoc : (unit : GameUnit) -> (to : Pos) -> GameError
  ForbiddenTerrain : (from : Pos) -> (to : Pos) -> GameError
  InvalidMove : (from : Pos) -> (to : Pos) -> GameError
  NotEnoughMPs : (unit : GameUnit) -> (from : Pos)-> (to : Pos) -> (mp : Nat) -> GameError
  NotAdjacentTo : (units : List GameUnit) -> (target : Pos) -> GameError
  NothingToAttack : (target : Pos) -> GameError
  AttackingOwnUnits : (units : List GameUnit) -> (target : Pos) -> GameError
  CombatInProgress : (side : Side) -> GameError
  GameHasEnded : GameError

Show GameError where
  show (NoSuchUnits unitNames) = "No such units: " ++ show unitNames
  show (NotYourTurn side) = "Not your turn: " ++ show side
  show (EnemyInHex unit hex) = "Target hex is occupied by enemy: " ++ show hex
  show (MoveFromZocToZoc unit to) = "Cannot move from a ZoC to a ZoC: " ++ show to
  show (ForbiddenTerrain from to) = "Unit cannot enter terrain: " ++ show from ++ " -> " ++ show to
  show (InvalidMove from to) = "Move is invalid: " ++ show from ++ " -> " ++ show to
  show (NotEnoughMPs unit from to mp) = "Unit has not enough MPs: " ++ show mp ++ ", " ++ show from ++ " -> " ++ show to
  show (NotAdjacentTo units target) = "Units are not adjacent to target hex: " ++ show units ++ " -> " ++ show target
  show (NothingToAttack target) = "Attacked hex is empty: " ++ show target
  show (AttackingOwnUnits units target) = "Attacking own units: " ++ show units ++ " -> " ++ show target
  show (CombatInProgress side) = "Combat in progress for: " ++ show side
  show GameHasEnded = "Game has ended"

data Command : (segment : GameSegment) -> Type where
  MoveTo : (unitName : String) -> (to : Pos) -> Command Move
  AttackWith : (unitNames : List String) -> (target : Pos) -> Command (Combat NoCombat)
  NextSegment : Command segment

Show (Command segment) where
  show (MoveTo unitName to) = "MoveTo " ++ unitName ++ " -> " ++ show to
  show (AttackWith unitNames target) = "AttackWith " ++ show unitNames ++ " -> " ++ show target
  show NextSegment = "NextSegment"

data Event : Type where

  ||| Unit has moved from some position to some other position
  Moved : (unit : GameUnit) -> (from : Pos) -> (to : Pos) -> (cost : Cost)
        -> { auto prf : LTE (toNat cost) (currentMP unit) }
        -> Event

  ||| Some attackers have engaged combat with some defenders on the given target hex.
  |||
  ||| The target hex can be inferred from the `defenders`' position, eg. all `defenders`
  ||| should be in the same hex.
  |||
  ||| @attackers list of attacker's units and their positions
  ||| @defenders list of defender's units and their positions
  ||| @target the attacked position
  CombatEngaged : (attackers : List (GameUnit, Pos)) -> (defenders : List (GameUnit, Pos)) -> (target : Pos) -> Event

  ||| The segment has been advanced one step.
  |||
  ||| @from the previous segment
  ||| @to the new segment
  SegmentChanged : (from : GameSegment)  -> (to : GameSegment) -> Event

  ||| Axis turn is over, move to Allies turn
  AxisTurnDone : Event

  ||| Turn ended, start a new turn
  TurnEnded : Fin 6 -> Event

  ||| Game has ended
  GameEnded : Event

Show Event where
  show (Moved unit from to cost) = "Moved " ++ name unit ++ " from " ++ show from ++ " to " ++ show to ++ " for "  ++ show (toNat cost) ++ " mps"
  show (CombatEngaged atk def tgt) = "CombatEngaged " ++ show (map (GameUnit.name . fst) atk) ++ " -> " ++ show (map (GameUnit.name . fst) def) ++ " @ " ++ show tgt
  show (SegmentChanged from to) = "Segment Changed " ++ show from ++ " -> " ++ show to
  show AxisTurnDone = "Axis Turn Over"
  show (TurnEnded n) = "Turn Ended: " ++ show (finToNat n)
  show GameEnded = "Game Ended"

data Game : Type where
  MkGame : (events : List Event) -> (curState : GameState) -> (gameMap : Map) -> Game

Show Game where
  show (MkGame events state gameMap) = "Game: " ++ show events ++ "\n" ++ show state ++ "\n" ++ show gameMap

curSegment : Game -> GameSegment
curSegment (MkGame _ (MkGameState _ _  segment _) _) = segment
