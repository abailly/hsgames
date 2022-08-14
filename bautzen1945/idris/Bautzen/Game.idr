module Bautzen.Game

import public Bautzen.Combats
import public Bautzen.GameUnit
import public Bautzen.Game.Combat
import public Bautzen.Game.Core
import public Bautzen.Game.Move
import public Bautzen.Game.Setup
import public Bautzen.Game.Supply
import public Bautzen.Game.Turn
import public Bautzen.Pos
import public Bautzen.Terrain
import public Bautzen.Game.Map

import Language.JSON
import Data.List
import Data.Fin
import Data.Nat

%default total

public
export
act : (game : Game) -> Command (curSegment game) -> Either GameError (Event (curSegment game))
act (MkGame (MkGameState _ side Setup units) gameMap) (Place unitName pos) = placeAt side units gameMap unitName pos
act (MkGame (MkGameState _ side Move units) gameMap) (MoveTo unitName to) = moveTo side units gameMap unitName to
act (MkGame (MkGameState _ side (Combat NoCombat) units) gameMap) (AttackWith unitNames target) = attackWith side units gameMap unitNames target
act game NextSegment = nextSegment game
act (MkGame (MkGameState _ side (Combat (AssignTacticalSupport combatSide combat)) units) gameMap) (TacticalSupport unitNames) =
  supportWith side combatSide units gameMap unitNames combat
act (MkGame (MkGameState _ side (Combat (Resolve combat)) units) gameMap) (ResolveCombat combat) =
  resolveCombat side combat
act (MkGame (MkGameState _ side (Combat (ApplyLosses lossSide combat)) units) gameMap) (LoseStep unitName) =
  loseStep side lossSide combat unitName

applyTacticalSupportEvent : Side -> CombatState -> List (GameUnit, Pos) -> GameState -> GameState
applyTacticalSupportEvent supportedSide (MkCombatState combatHex attackers defenders losses) units game =
  if side game == supportedSide
  then { stateSegment := Combat (AssignTacticalSupport (flipSide supportedSide)
                                       (MkCombatState combatHex
                                        ({ tacticalSupport := units } attackers)
                                        defenders
                                        losses)) } game
  else { stateSegment := Combat (AssignStrategicSupport (flipSide supportedSide)
                                       (MkCombatState combatHex
                                        attackers
                                        ({ tacticalSupport := units } defenders)
                                        losses)) } game

applySupplyColumnUsedEvent :  Side -> CombatState -> Pos -> GameState -> GameState
applySupplyColumnUsedEvent supportedSide (MkCombatState combatHex attackers defenders losses) hex game =
  if side game == supportedSide
  then { stateSegment := Combat (AssignStrategicSupport supportedSide
                                       (MkCombatState combatHex
                                        ({ strategicSupport $= (+1) } attackers)
                                        defenders
                                        losses)) } game
  else { stateSegment := Combat (AssignStrategicSupport supportedSide
                                       (MkCombatState combatHex
                                        attackers
                                        ({ strategicSupport $= (+1) } defenders)
                                        losses)) } game

-- TODO handle case where there are more losses to applies than units
applyStepLostEvent :
  Side -> GameUnit -> Losses -> CombatState -> GameState -> GameState
applyStepLostEvent lossSide unit newLosses state game@(MkGameState turn side segment units) =
  { stateSegment := newSegment, units := reduced } game
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


applyEvent : (st : GameState) -> Event (st.stateSegment) -> GameState
applyEvent (MkGameState turn side Setup units) (Placed unit pos) =
  MkGameState turn side Setup (updatePlacedUnit unit pos units)
applyEvent (MkGameState turn _ Setup us) AlliesSetupDone =
   (MkGameState turn Axis Setup us)
applyEvent (MkGameState turn side Move units) (Moved unit from to cost) =
  MkGameState turn side Move (updateMovedUnit unit to (Terrain.toNat cost) units)
applyEvent game@(MkGameState turn side (Combat NoCombat) units) (CombatEngaged atk def tgt) =
  { stateSegment := Combat (AssignTacticalSupport side
                     (MkCombatState tgt
                       (MkEngagedUnits atk [] 0)
                       (MkEngagedUnits def [] 0)
                       Nothing)) } game
applyEvent (MkGameState turn side (Combat (AssignTacticalSupport supSide combat)) us) (TacticalSupportProvided supSide units) =
  -- NOTE: https://github.com/idris-lang/Idris2/issues/490
  -- I would love to write 'game@...' but it does not currently work
  applyTacticalSupportEvent supSide combat units (MkGameState turn side (Combat (AssignTacticalSupport supSide combat)) us)
applyEvent (MkGameState turn side (Combat (AssignStrategicSupport supSide combat)) us) (SupplyColumnUsed supSide pos) =
  applySupplyColumnUsedEvent supSide combat pos (MkGameState turn side (Combat (AssignStrategicSupport supSide combat)) us)
applyEvent (MkGameState turn side (Combat (Resolve st)) us) (CombatResolved state losses) =
  MkGameState turn side (Combat $ ApplyLosses side ({ losses := Just losses } state)) us
applyEvent (MkGameState turn side (Combat (ApplyLosses lossSide combatState)) us) (StepLost lossSide unit newLosses) =
   applyStepLostEvent lossSide unit newLosses combatState (MkGameState turn side (Combat (ApplyLosses lossSide combatState)) us)
applyEvent game (SegmentChanged (game.stateSegment) to) =
   { stateSegment := to } game
applyEvent (MkGameState turn side (Combat NoCombat) us) AxisTurnDone =
   (MkGameState turn Allies Supply us)
applyEvent (MkGameState turn side (Combat NoCombat) us) (TurnEnded n) =
   (MkGameState n Axis Supply us)
applyEvent game GameEnded =
   { stateSegment := GameEnd } game

export
apply : (game : Game) -> Event (curSegment game) -> Game
apply game event =
  MkGame (applyEvent game.curState event) game.gameMap


-- Queries

public
export
record CurrentGameSegment where
  constructor MkCurrentGameSegment
  turn : Fin 6
  side : Side
  gameSegment : GameSegment

export
makeCurrentSegment : Game -> CurrentGameSegment
makeCurrentSegment (MkGame (MkGameState turn side segment _) _) = MkCurrentGameSegment turn side segment

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
  Positions : Query AllPositions

  ||| Retrieve the stage (turn, side, segment) the game is currently at
  GetCurrentSegment : Query CurrentGameSegment

export
Show (Query a) where
  show (SupplyPath u) = "SupplyPath " ++ show u
  show TerrainMap = "TerrainMap"
  show Positions = "Positions"
  show GetCurrentSegment = "GetCurrentSegment"

export
query : (game : Game) -> (qry : Query result) -> result
query (MkGame (MkGameState _ side _ units) gameMap) (SupplyPath unitName) =
  case find (sameName unitName) units of
    Nothing => Left (UnitDoesNotExist unitName)
    (Just (unit, pos)) =>
      case supplyPathTo units gameMap (supplySources (nation unit) gameMap) (unit, pos) of
        [] => Left (NoSupplyPathFor unitName pos)
        x  => Right x
query (MkGame (MkGameState _ side _ units) gameMap) TerrainMap = gameMap
query (MkGame (MkGameState _ side _ positions) _) Positions = MkAllPositions positions
query game GetCurrentSegment = makeCurrentSegment game

||| A single player action, either a @Command@ or a @Query@.
public export
data PlayerAction : (seg : GameSegment) -> Type where
  Cmd : (cmd : Command seg) -> PlayerAction seg
  Qry : Cast res JSON => (qry : Query res) -> PlayerAction seg

public export
data ActionResult : (seg : GameSegment) -> Type where
  ResEvent : Event seg -> ActionResult seg
  ResError : GameError -> ActionResult seg
  ResQuery : Cast result JSON => result -> ActionResult seg

export
Show (ActionResult seg) where
  show (ResEvent x) = "ResEvent " ++ show x
  show (ResError x) = "ResError " ++ show x
  show (ResQuery x) = "ResQuery " ++ show (cast { to = JSON} x)

export
Eq (ActionResult seg) where
  (ResEvent x) == (ResEvent y) = x == y
  (ResError x) == (ResError y) = x == y
  -- assume JSON representation is injective
  (ResQuery x) == (ResQuery y) = show (cast {to = JSON} x) == show (cast {to = JSON} y)
  _ == _ = False

public export
handleAction : (game : Game) -> PlayerAction (curSegment game) -> ActionResult (curSegment game)
handleAction game (Cmd cmd) =
  case act game cmd of
      Left err => ResError err
      Right event => ResEvent event
handleAction game (Qry qry) =
  let qryResult = query game qry
  in ResQuery qryResult

public export
applyResult : (game : Game) -> ActionResult (curSegment game) -> Game
applyResult g (ResEvent x) = apply g x
applyResult g (ResError x) = g
applyResult g (ResQuery x) = g

export
initialPositions : List (GameUnit, Pos)
initialPositions = [ (Bautzen.GameUnit.p13_5dp, hex 1 9)
                   , (Bautzen.GameUnit.g21_20pz, hex 5 8)
                   ]

export
initialState : GameState
initialState = MkGameState 5 Allies Setup initialPositions

export
initialGame : Game
initialGame = MkGame initialState FullGameMap
