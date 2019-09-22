module Bautzen.Game.Core

import Bautzen.GameUnit
import Bautzen.Pos
import Bautzen.Terrain

import Data.Fin

%access public export
%default total

data GameSegment : Type where
  Supply : GameSegment
  Move : GameSegment
  Combat : GameSegment

Show GameSegment where
  show Supply = "Supply"
  show Move = "Move"
  show Combat = "Combat"

record GameState where
  constructor MkGameState
  turn : Fin 5
  side : Side
  segment : GameSegment
  units : List (GameUnit, Pos)

Show GameState where
  show (MkGameState turn side segment units) = "GameState: turn=" ++ show (finToInteger turn) ++ ", side=" ++ show side ++ ", "++ show segment ++ ", units=" ++ show units

data GameError : Type where
  NoSuchUnits : (unitName : List String) -> GameError
  NotYourTurn : (side : Side) -> GameError
  EnemyInHex : (unit : GameUnit) -> (hex : Pos) -> GameError
  MoveFromZocToZoc : (unit : GameUnit) -> (to : Pos) -> GameError
  ForbiddenTerrain : (from : Pos) -> (to : Pos) -> GameError
  InvalidMove : (from : Pos) -> (to : Pos) -> GameError
  NotEnoughMPs : (unit : GameUnit) -> (from : Pos)-> (to : Pos) -> (mp : Nat) -> GameError
  NotAdjacentTo : (units : List GameUnit) -> (target : Pos) -> GameError

Show GameError where
  show (NoSuchUnits unitNames) = "No such units: " ++ show unitNames
  show (NotYourTurn side) = "Not your turn: " ++ show side
  show (EnemyInHex unit hex) = "Target hex is occupied by enemy: " ++ show hex
  show (MoveFromZocToZoc unit to) = "Cannot move from a ZoC to a ZoC: " ++ show to
  show (ForbiddenTerrain from to) = "Unit cannot enter terrain: " ++ show from ++ " -> " ++ show to
  show (InvalidMove from to) = "Move is invalid: " ++ show from ++ " -> " ++ show to
  show (NotEnoughMPs unit from to mp) = "Unit has not enough MPs: " ++ show mp ++ ", " ++ show from ++ " -> " ++ show to
  show (NotAdjacentTo units target) = "Units are not adjacent to target hex: " ++ show units ++ " -> " ++ show target

data Command : (segment : GameSegment) -> Type where
  MoveTo : (unitName : String) -> (to : Pos) -> Command Move
  AttackWith : (unitNames : List String) -> (target : Pos) -> Command Combat

data Event : Type where
  ||| Unit has moved from some position to some other position
  Moved : (unit : GameUnit) -> (from : Pos) -> (to : Pos) -> (cost : Cost)
        -> { auto prf : LTE (toNat cost) (currentMP unit) }
        -> Event

Show Event where
  show (Moved unit from to cost) = "Moved " ++ name unit ++ " from " ++ show from ++ " to " ++ show to ++ " for "  ++ show (toNat cost) ++ " mps"

data Game : Type where
  MkGame : (events : List Event) -> (curState : GameState) -> (gameMap : Map) -> Game

Show Game where
  show (MkGame events state gameMap) = "Game: " ++ show events ++ "\n" ++ show state ++ "\n" ++ show gameMap

curSegment : Game -> GameSegment
curSegment (MkGame _ (MkGameState _ _  segment _) _) = segment
