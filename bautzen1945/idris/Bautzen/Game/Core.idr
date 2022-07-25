module Bautzen.Game.Core

import Bautzen.Combats
import Bautzen.GameUnit
import Bautzen.Terrain

import Data.Fin
import Data.Nat
import Decidable.Equality

%default total

||| * see section 8.2
public export
data CombatPhase : Type where
  NoCombat : CombatPhase
  AssignTacticalSupport : (side : Side) -> (combat : CombatState) -> CombatPhase
  AssignStrategicSupport : (side : Side) -> (combat : CombatState) -> CombatPhase
  Resolve : (combat : CombatState) -> CombatPhase
  ApplyLosses : (side : Side) -> (combat : CombatState) -> CombatPhase

sideTacSupportInjective : (AssignTacticalSupport side combat = AssignTacticalSupport side' combat') -> (side = side')
sideTacSupportInjective Refl = Refl

combatTacSupportInjective : (AssignTacticalSupport side combat = AssignTacticalSupport side' combat') -> (combat = combat')
combatTacSupportInjective Refl = Refl

sideStratSupportInjective : (AssignStrategicSupport side combat = AssignStrategicSupport side' combat') -> (side = side')
sideStratSupportInjective Refl = Refl

combatStratSupportInjective : (AssignStrategicSupport side combat = AssignStrategicSupport side' combat') -> (combat = combat')
combatStratSupportInjective Refl = Refl

sideLossesInjective : (ApplyLosses side combat = ApplyLosses side' combat') -> (side = side')
sideLossesInjective Refl = Refl

combatLossesInjective : (ApplyLosses side combat = ApplyLosses side' combat') -> (combat = combat')
combatLossesInjective Refl = Refl

combatResolveInjective : (Resolve combat = Resolve combat') -> (combat = combat')
combatResolveInjective Refl = Refl

export
DecEq CombatPhase where
  decEq NoCombat NoCombat = Yes Refl
  decEq (AssignTacticalSupport side combat) (AssignTacticalSupport side' combat') with (decEq side side')
    decEq (AssignTacticalSupport side combat) (AssignTacticalSupport side' combat') | (Yes prf) with (decEq @{FromEq} combat combat')
      decEq (AssignTacticalSupport side combat) (AssignTacticalSupport side' combat') | (Yes prf) | (Yes x) = rewrite prf in rewrite x in Yes Refl
      decEq (AssignTacticalSupport side combat) (AssignTacticalSupport side' combat') | (Yes prf) | (No contra) = No $ contra . combatTacSupportInjective
    decEq (AssignTacticalSupport side combat) (AssignTacticalSupport side' combat') | (No contra) = No $ contra . sideTacSupportInjective
  decEq (AssignStrategicSupport side combat) (AssignStrategicSupport side' combat') with (decEq side side')
    decEq (AssignStrategicSupport side combat) (AssignStrategicSupport side' combat') | (Yes prf) with (decEq @{FromEq} combat combat')
      decEq (AssignStrategicSupport side combat) (AssignStrategicSupport side' combat') | (Yes prf) | (Yes x) = rewrite prf in rewrite x in Yes Refl
      decEq (AssignStrategicSupport side combat) (AssignStrategicSupport side' combat') | (Yes prf) | (No contra) = No $ contra . combatStratSupportInjective
    decEq (AssignStrategicSupport side combat) (AssignStrategicSupport side' combat') | (No contra) = No $ contra . sideStratSupportInjective
  decEq (Resolve combat) (Resolve combat') with (decEq @{FromEq} combat combat')
    decEq (Resolve combat) (Resolve combat') | (Yes prf) = rewrite prf in Yes Refl
    decEq (Resolve combat) (Resolve combat') | (No contra) = No $ contra . combatResolveInjective
  decEq (ApplyLosses side combat) (ApplyLosses side' combat') with (decEq side side')
    decEq (ApplyLosses side combat) (ApplyLosses side' combat') | (Yes prf) with (decEq @{FromEq} combat combat')
      decEq (ApplyLosses side combat) (ApplyLosses side' combat') | (Yes prf) | (Yes x) = rewrite x in rewrite prf in Yes Refl
      decEq (ApplyLosses side combat) (ApplyLosses side' combat') | (Yes prf) | (No contra) = No $ contra . combatLossesInjective
    decEq (ApplyLosses side combat) (ApplyLosses side' combat') | (No contra) = No $ contra . sideLossesInjective
  decEq x x' = No believe_me

public export
data GameSegment : Type where
  Supply : GameSegment
  Move : GameSegment
  Combat : (phase : CombatPhase) -> GameSegment
  GameEnd : GameSegment

combatInjective : Combat c = Combat c' -> c = c'
combatInjective Refl = Refl

public export
DecEq GameSegment where
  decEq Supply Supply = Yes Refl
  decEq Move Move = Yes Refl
  decEq (Combat phase) (Combat phase') = case decEq phase phase' of
     Yes prf => Yes $ cong Combat prf
     No contra => No $ \ c => contra (combatInjective c)
  decEq GameEnd GameEnd  = Yes Refl
   -- TODO: Not sure how to prove all those negative cases
  decEq x x' = No believe_me

public export
Show GameSegment where
  show Supply = "Supply"
  show Move = "Move"
  show (Combat _) = "Combat"
  show GameEnd = "GameEnd"

public export
record GameState where
  constructor MkGameState
  turn : Fin 6
  side : Side
  stateSegment : GameSegment
  units : List (GameUnit, Pos)

public export
Show GameState where
  show (MkGameState turn side segment units) =
    "GameState: turn=" ++
    show (finToInteger turn) ++
    ", side=" ++ show side ++
    ", " ++ show segment ++
    ", units=" ++ show units

public export
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
  NotSupportingUnits: (units : List GameUnit) -> GameError
  NotInSupportRange: (units : List GameUnit) -> GameError
  NotInChainOfCommand : (units : List GameUnit) -> GameError
  NoSupplyColumnThere : (hex : Pos) -> GameError
  NoStepsToLose : (side : Side) -> GameError
  CombatInProgress : (side : Side) -> GameError
  GameHasEnded : GameError

public export
Show GameError where
  show (NoSuchUnits unitNames) = "No such units: " ++ show unitNames
  show (NotYourTurn side) = "Not your turn: " ++ show side
  show (EnemyInHex unit hex) = "Target hex is occupied by enemy: " ++ show hex
  show (MoveFromZocToZoc unit to) = "Cannot move from a ZoC to a ZoC: " ++ show to
  show (ForbiddenTerrain from to) = "Unit cannot enter terrain: " ++ show from ++ " -> " ++ show to
  show (InvalidMove from to) = "Move is invalid: " ++ show from ++ " -> " ++ show to
  show (NotEnoughMPs unit from to mp) = "Unit has not enough MPs: " ++ show mp ++ ", " ++ show from ++ " -> " ++ show to
  show (NotAdjacentTo units target) = "Units are not adjacent to target hex: " ++ show units ++ " -> " ++ show target
  show (NotInSupportRange units) = "Units are not in support range: " ++ show units
  show (NotSupportingUnits units) = "Units are not support units (HQ or Artillery): " ++ show units
  show (NotInChainOfCommand units) = "HQ cannot provide support to other formation's units: " ++ show units
  show (NoSupplyColumnThere hex) = "No supply column there: " ++ show hex
  show (NoStepsToLose side) = "No steps to lose for " ++ show side
  show (NothingToAttack target) = "Attacked hex is empty: " ++ show target
  show (AttackingOwnUnits units target) = "Attacking own units: " ++ show units ++ " -> " ++ show target
  show (CombatInProgress side) = "Combat in progress for: " ++ show side
  show GameHasEnded = "Game has ended"

public export
data Command : (segment : GameSegment) -> Type where
  MoveTo : (unitName : String) -> (to : Pos) -> Command Move
  AttackWith : (unitNames : List String) -> (target : Pos) -> Command (Combat NoCombat)
  NextSegment : Command segment
  TacticalSupport : (unitNames : List String) -> Command (Combat $ AssignTacticalSupport side combatState)
  ResolveCombat : (combatState : CombatState) -> Command (Combat $ Resolve combatState)
  LoseStep : (unitName : String) -> Command (Combat $ ApplyLosses side combatState)

public export
Show (Command segment) where
  show (MoveTo unitName to) = "MoveTo " ++ unitName ++ " -> " ++ show to
  show (AttackWith unitNames target) = "AttackWith " ++ show unitNames ++ " -> " ++ show target
  show NextSegment = "NextSegment"
  show (TacticalSupport unitNames) = "TacticalSupport " ++ show unitNames
  show (ResolveCombat state) = "ResolveCombat " ++ show state
  show (LoseStep unitName) = "LoseStep " ++ show unitName

public export
data Event : (segment : GameSegment) -> Type where

  ||| Unit has moved from some position to some other position
  Moved : (unit : GameUnit) -> (from : Pos) -> (to : Pos) -> (cost : Cost)
        -> { auto prf : LTE (toNat cost) (currentMP unit) }
        -> Event Move

  ||| Some attackers have engaged combat with some defenders on the given target hex.
  |||
  ||| The target hex can be inferred from the `defenders`' position, eg. all `defenders`
  ||| should be in the same hex.
  |||
  ||| @attackers list of attacker's units and their positions
  ||| @defenders list of defender's units and their positions
  ||| @target the attacked position
  CombatEngaged : (attackers : List (GameUnit, Pos)) -> (defenders : List (GameUnit, Pos)) -> (target : Pos) -> Event (Combat NoCombat)

  ||| Units provide tactical support for some side in current combat
  |||
  ||| @supportedSide the side which is given support
  ||| @supportUnits units and positions that provide support
  TacticalSupportProvided : (supportedSide : Side) -> (supportUnits : List (GameUnit, Pos)) -> Event (Combat $ AssignTacticalSupport supportedSide combatState)

  ||| A supply column is used to provide support to units engaged in a combat
  |||
  ||| @supportedSide the side which is given support
  ||| @hex the position of the supply column
  SupplyColumnUsed : (supportedSide : Side) -> (hex : Pos) -> Event (Combat $ AssignStrategicSupport supportedSide combatState)

  ||| Combat has been resolved yielding the given losses
  |||
  ||| @state the state of the combat (before any loss has been applied)
  ||| @losses losses to apply on engaged units
  CombatResolved : (state : CombatState) -> (losses : Losses) -> Event (Combat $ Resolve combatState)

  ||| Given unit has lost a step
  |||
  ||| Depending on the current state of the unit, this can either
  ||| either reduce the unit or destroy it.
  |||
  ||| @side the side the unit is part of (useful to look for the unit in
  ||| the combat state
  ||| @unit the unit to apply a step loss to
  ||| @remainingLosses  losses  remaining to apply
  StepLost : (side : Side) -> (unit : GameUnit) -> (remainingLosses : Losses) -> Event  (Combat $ ApplyLosses side combatState)

  ||| The segment has been advanced one step.
  |||
  ||| @from the previous segment
  ||| @to the new segment
  SegmentChanged : (from : GameSegment)  -> (to : GameSegment) -> Event from

  ||| Axis turn is over, move to Allies turn
  AxisTurnDone : Event (Combat NoCombat)

  ||| Turn ended, start a new turn
  TurnEnded : Fin 6 -> Event (Combat NoCombat)

  ||| Game has ended
  GameEnded : Event segment

public export
Show (Event seg) where
  show (Moved unit from to cost) = "Moved " ++ name unit ++ " from " ++ show from ++ " to " ++ show to ++ " for "  ++ show (toNat cost) ++ " mps"
  show (CombatEngaged atk def tgt) = "CombatEngaged " ++ show (map (GameUnit.name . fst) atk) ++ " -> " ++ show (map (GameUnit.name . fst) def) ++ " @ " ++ show tgt
  show (TacticalSupportProvided side units) = "TacticalSupportProvided " ++ show (map (GameUnit.name . fst) units) ++ " -> " ++ show side
  show (SupplyColumnUsed side hex) = "SupplyColumnUsed " ++ show side ++ " @ " ++ show hex
  show (CombatResolved state losses) = "CombatResolved " ++ show state ++ " : " ++ show losses
  show (StepLost side unit remain) = "Step Lost " ++ show unit ++ " @ " ++ show side ++  " (" ++ show remain++")"
  show (SegmentChanged from to) = "Segment Changed " ++ show from ++ " -> " ++ show to
  show AxisTurnDone = "Axis Turn Over"
  show (TurnEnded n) = "Turn Ended: " ++ show (finToNat n)
  show GameEnded = "Game Ended"

public export
record Game  where
  constructor  MkGame
  curState : GameState
  gameMap : Map

public export
Show Game where
  show (MkGame state gameMap) = "Game: " ++ show state ++ "\n" ++ show gameMap

public export
curSegment : Game -> GameSegment
curSegment game = game.curState.stateSegment

public export
data QueryError : Type where
  NoSupplyPathFor : (unitName: String) -> (pos : Pos) -> QueryError
  UnitDoesNotExist : (unitName: String) -> QueryError

public export
record AllPositions where
  constructor MkAllPositions
  positions : List (GameUnit, Pos)
