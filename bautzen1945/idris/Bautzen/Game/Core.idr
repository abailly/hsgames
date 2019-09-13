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

record GameState where
  constructor MkGameState
  turn : Fin 5
  side : Side
  segment : GameSegment
  units : List (GameUnit, Pos)

data GameError : Type where
  NoSuchUnit : (unitName : String) -> GameError
  NotYourTurn : (side : Side) -> GameError
  EnemyInHex : (unit : GameUnit) -> (hex : Pos) -> GameError
  MoveFromZocToZoc : (unit : GameUnit) -> (to : Pos) -> GameError
  ForbiddenTerrain : (from : Pos) -> (to : Pos) -> GameError
  NotEnoughMPs : (unit : GameUnit) -> (from : Pos)-> (to : Pos) -> (mp : Nat) -> GameError

data Command : (segment : GameSegment) -> Type where
  MoveTo : (unitName : String) -> (to : Pos) -> Command Move

data Event : Type where
  ||| Unit has moved from some position to some other position
  Moved : (unit : GameUnit) -> (from : Pos) -> (to : Pos) -> (cost : Cost)
        -> { auto prf : LTE (toNat cost) (currentMP unit) }
        -> Event

data Game : Type where
  MkGame : (events : List Event) -> (curState : GameState) -> Game

curSegment : Game -> GameSegment
curSegment (MkGame events (MkGameState turn side segment units)) = segment

initialState : GameState
initialState = MkGameState 0 Axis Supply []

initialGame : Game
initialGame = MkGame [] initialState
