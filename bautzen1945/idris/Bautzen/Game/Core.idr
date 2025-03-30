module Bautzen.Game.Core

import Bautzen.Combats
import Bautzen.GameUnit
import Bautzen.Terrain

import Data.Fin
import Data.Nat
import Decidable.Decidable
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
  decEq NoCombat (AssignTacticalSupport _ _) = No $ \case Refl impossible
  decEq NoCombat (AssignStrategicSupport _ _) = No $ \case Refl impossible
  decEq NoCombat (Resolve _) = No $ \case Refl impossible
  decEq NoCombat (ApplyLosses _ _) = No $ \case Refl impossible
  decEq (AssignTacticalSupport _ _) NoCombat = No $ \case Refl impossible
  decEq (AssignTacticalSupport _ _) (AssignStrategicSupport _ _) = No $ \case Refl impossible
  decEq (AssignTacticalSupport _ _) (Resolve _) = No $ \case Refl impossible
  decEq (AssignTacticalSupport _ _) (ApplyLosses _ _) = No $ \case Refl impossible
  decEq (AssignStrategicSupport _ _) NoCombat = No $ \case Refl impossible
  decEq (AssignStrategicSupport _ _) (AssignTacticalSupport _ _) = No $ \case Refl impossible
  decEq (AssignStrategicSupport _ _) (Resolve _) = No $ \case Refl impossible
  decEq (AssignStrategicSupport _ _) (ApplyLosses _ _) = No $ \case Refl impossible
  decEq (Resolve _) NoCombat = No $ \case Refl impossible
  decEq (Resolve _) (AssignTacticalSupport _ _) = No $ \case Refl impossible
  decEq (Resolve _) (AssignStrategicSupport _ _) = No $ \case Refl impossible
  decEq (Resolve _) (ApplyLosses _ _) = No $ \case Refl impossible
  decEq (ApplyLosses _ _) NoCombat = No $ \case Refl impossible
  decEq (ApplyLosses _ _) (AssignTacticalSupport _ _) = No $ \case Refl impossible
  decEq (ApplyLosses _ _) (AssignStrategicSupport _ _) = No $ \case Refl impossible
  decEq (ApplyLosses _ _) (Resolve _) = No $ \case Refl impossible

public export
data GameSegment : Type where
  Setup : GameSegment
  Supply : GameSegment
  Move : GameSegment
  Combat : (phase : CombatPhase) -> GameSegment
  GameEnd : GameSegment

combatInjective : Combat c = Combat c' -> c = c'
combatInjective Refl = Refl

public export
DecEq GameSegment where
  decEq Setup Setup = Yes Refl
  decEq Supply Supply = Yes Refl
  decEq Move Move = Yes Refl
  decEq (Combat phase) (Combat phase') = case decEq phase phase' of
     Yes prf => Yes $ cong Combat prf
     No contra => No $ \ c => contra (combatInjective c)
  decEq GameEnd GameEnd  = Yes Refl
   -- TODO: Not sure how to prove all those negative cases
  decEq x x' = No believe_me

export
Eq GameSegment where
  seg == seg' = isYes (decEq seg seg')

public export
Show GameSegment where
  show Setup = "Setup"
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

public
export
Eq GameState where
  MkGameState turn side seg units == MkGameState turn' side' seg' units' =
    turn == turn' &&
    side == side' &&
    seg == seg' &&
    units == units'

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
  InvalidPlacement : (pos : Pos) -> GameError
  GameHasEnded : GameError

export
Eq GameError where
  (NoSuchUnits unitName) ==   (NoSuchUnits unitName')  =
    unitName == unitName'
  (NotYourTurn side) ==   (NotYourTurn side')  =
      side == side'
  (EnemyInHex unit hex) ==   (EnemyInHex unit' hex')  =
     unit == unit' && hex == hex'
  (MoveFromZocToZoc unit to) ==   (MoveFromZocToZoc unit' to')  =
     unit == unit' && to == to'
  (ForbiddenTerrain from to) ==   (ForbiddenTerrain from' to')  =
     from == from' && to == to'
  (InvalidMove from to) ==   (InvalidMove from' to')  =
     from == from' && to == to'
  (NotEnoughMPs unit from to mp) ==   (NotEnoughMPs unit' from' to' mp')  =
     unit == unit' && from == from' && to == to' && mp == mp'
  (NotAdjacentTo units target) ==   (NotAdjacentTo units' target')  =
     units == units' && target == target'
  (NothingToAttack target) ==   (NothingToAttack target')  =
     target == target'
  (AttackingOwnUnits units target) ==   (AttackingOwnUnits units' target')  =
     units == units' && target == target'
  (NotSupportingUnits units) ==   (NotSupportingUnits units')  =
     units == units'
  (NotInSupportRange units) ==   (NotInSupportRange units')  =
     units == units'
  (NotInChainOfCommand units) ==   (NotInChainOfCommand units')  =
     units == units'
  (NoSupplyColumnThere hex) ==   (NoSupplyColumnThere hex')  =
     hex == hex'
  (NoStepsToLose side) ==   (NoStepsToLose side')  =
     side == side'
  (CombatInProgress side) ==   (CombatInProgress side')  =
     side == side'
  (InvalidPlacement pos) ==   (InvalidPlacement pos')  =
     pos == pos'
  GameHasEnded ==   GameHasEnded  = True
  _ == _ = False

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
  show (InvalidPlacement pos) = "Placement is invalid: " ++ show pos
  show GameHasEnded = "Game has ended"

-- TODO: there is no need, and it's logically inconsistent, to index the `Command` type
-- with `GameSegment`: A Command from the external world and there's no guarantee it applies
-- to some segment, something which is verified by the `act` function
public export
data Command : (segment : GameSegment) -> Type where
  Place : (unitName : String) -> (pos : Pos) -> Command Setup
  MoveTo : (unitName : String) -> (to : Pos) -> Command Move
  AttackWith : (unitNames : List String) -> (target : Pos) -> Command (Combat NoCombat)
  NextSegment : Command segment
  TacticalSupport : (unitNames : List String) -> Command (Combat $ AssignTacticalSupport side combatState)
  ResolveCombat : (combatState : CombatState) -> Command (Combat $ Resolve combatState)
  LoseStep : (unitName : String) -> Command (Combat $ ApplyLosses side combatState)

public export
Show (Command segment) where
  show (Place unitName to) = "Place " ++ unitName ++ " @ " ++ show to
  show (MoveTo unitName to) = "MoveTo " ++ unitName ++ " -> " ++ show to
  show (AttackWith unitNames target) = "AttackWith " ++ show unitNames ++ " -> " ++ show target
  show NextSegment = "NextSegment"
  show (TacticalSupport unitNames) = "TacticalSupport " ++ show unitNames
  show (ResolveCombat state) = "ResolveCombat " ++ show state
  show (LoseStep unitName) = "LoseStep " ++ show unitName

public export
data Event : (segment : GameSegment) -> Type where

  ||| Unit has been placed at given location
  Placed : (unit : GameUnit) -> (pos : Pos) -> Event Setup

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
  CombatResolved : (state : CombatState) -> (losses : Losses) -> Event (Combat $ Resolve state)

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

  ||| Allies setup done.
  AlliesSetupDone : Event Setup

  ||| Turn ended, start a new turn
  TurnEnded : Fin 6 -> Event (Combat NoCombat)

  ||| Game has ended
  GameEnded : Event segment

export
Eq (Event seg) where
  (Placed unit pos) == (Placed unit' pos')  =
    unit == unit' && pos == pos'
  (Moved unit from to cost) == (Moved unit' from' to' cost')  =
    unit == unit' && from == from' && to == to' && cost == cost'
  (CombatEngaged attackers defenders target) == (CombatEngaged attackers' defenders' target')  =
    attackers == attackers' && defenders == defenders' && target == target'
  (TacticalSupportProvided supportedSide supportUnits) == (TacticalSupportProvided supportedSide supportUnits')  =
    supportUnits == supportUnits'
  (SupplyColumnUsed supportedSide hex) == (SupplyColumnUsed supportedSide hex')  =
    hex == hex'
  (CombatResolved state losses) == (CombatResolved state losses')  =
    losses == losses'
  (StepLost side unit remainingLosses) == (StepLost side unit' remainingLosses')  =
    unit == unit' && remainingLosses == remainingLosses'
  (SegmentChanged seg to) == (SegmentChanged seg to')  =
    to == to'
  AxisTurnDone == AxisTurnDone  = True
  AlliesSetupDone == AlliesSetupDone  = True
  (TurnEnded x) == (TurnEnded x')  = x == x'
  GameEnded == GameEnded  = True
  _ == _ = False

public export
Show (Event seg) where
  show (Placed unit pos) = "Placed " ++ name unit ++ " at " ++ show pos
  show (Moved unit from to cost) = "Moved " ++ name unit ++ " from " ++ show from ++ " to " ++ show to ++ " for "  ++ show (toNat cost) ++ " mps"
  show (CombatEngaged atk def tgt) = "CombatEngaged " ++ show (map (GameUnit.name . fst) atk) ++ " -> " ++ show (map (GameUnit.name . fst) def) ++ " @ " ++ show tgt
  show (TacticalSupportProvided side units) = "TacticalSupportProvided " ++ show (map (GameUnit.name . fst) units) ++ " -> " ++ show side
  show (SupplyColumnUsed side hex) = "SupplyColumnUsed " ++ show side ++ " @ " ++ show hex
  show (CombatResolved state losses) = "CombatResolved " ++ show state ++ " : " ++ show losses
  show (StepLost side unit remain) = "Step Lost " ++ show unit ++ " @ " ++ show side ++  " (" ++ show remain++")"
  show (SegmentChanged from to) = "Segment Changed " ++ show from ++ " -> " ++ show to
  show AxisTurnDone = "Axis Turn Over"
  show AlliesSetupDone = "Allies Set-up Done"
  show (TurnEnded n) = "Turn Ended: " ++ show (finToNat n)
  show GameEnded = "Game Ended"

public export
data AnyEvent : Type where
  MkAnyEvent : { seg : GameSegment } -> Event seg -> AnyEvent

export
Eq AnyEvent where
  (MkAnyEvent x@(Placed _ _)) == (MkAnyEvent y@(Placed _ _)) = x == y
  (MkAnyEvent x@(Moved _ _ _ _)) == (MkAnyEvent y@(Moved _ _ _ _)) = x == y
  (MkAnyEvent x@(CombatEngaged _ _ _)) == (MkAnyEvent y@(CombatEngaged _ _ _)) = x == y
  (MkAnyEvent (TacticalSupportProvided side units)) == (MkAnyEvent (TacticalSupportProvided side' units')) = side == side' && units == units'
  (MkAnyEvent (SupplyColumnUsed side hex)) == (MkAnyEvent (SupplyColumnUsed side' hex')) = side == side' && hex == hex'
  (MkAnyEvent (CombatResolved step losses)) == (MkAnyEvent (CombatResolved step' losses')) = step == step' && losses == losses'
  (MkAnyEvent (StepLost side unit remain)) == (MkAnyEvent (StepLost side' unit' remain')) = side == side' && unit == unit' && remain == remain'
  (MkAnyEvent (SegmentChanged from to)) == (MkAnyEvent (SegmentChanged from' to')) = from == from' && to == to'
  (MkAnyEvent AxisTurnDone) == (MkAnyEvent AxisTurnDone) = True
  (MkAnyEvent AlliesSetupDone) == (MkAnyEvent AlliesSetupDone) = True
  (MkAnyEvent x@(TurnEnded _)) == (MkAnyEvent y@(TurnEnded _)) = x == y
  (MkAnyEvent GameEnded) == (MkAnyEvent GameEnded) = True
  _ == _ = False

export
Show AnyEvent where
  show (MkAnyEvent e) = show e

public export
record Game  where
  constructor  MkGame
  curState : GameState
  gameMap : Map

public export
Eq Game where
  MkGame s m == MkGame s' m' = s == s' && m == m'

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
