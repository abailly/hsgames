module Bautzen.Game

import public Bautzen.GameUnit
import public Bautzen.Game.Core
import public Bautzen.Game.Move
import public Bautzen.Game.Supply
import public Bautzen.Pos
import public Bautzen.Terrain
import public Bautzen.Game.Map

import Bautzen.SExp
import Bautzen.REPL.SExpInstances

import Data.Fin

%access export

%default total

act : (game : Game) -> Command (curSegment game) -> Either GameError Event
act (MkGame events (MkGameState turn side Move units)) (MoveTo unitName to) = moveTo side units TestMap unitName to

apply : Event -> Game -> Game
apply event (MkGame events curState) =
  MkGame (event :: events) (applyEvent event curState)
  where
    applyEvent : Event -> GameState -> GameState
    applyEvent (Moved unit from to cost) (MkGameState turn side segment units) =
      MkGameState turn side segment (updateMovedUnit unit to (toNat cost) units)

-- Queries

data QueryError : Type where
  NoSupplyPathFor : (unitName: String) -> (pos : Pos) -> QueryError
  UnitDoesNotExist : (unitName: String) -> QueryError

ToSExp QueryError where
  toSExp (NoSupplyPathFor unitName pos) = SList [ SSym "NoSupplyPathFor", SStr unitName, toSExp pos ]
  toSExp (UnitDoesNotExist unitName) = SList [ SSym "UnitDoesNotExist", SStr unitName ]

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

partial
query : (ToSExp result) => (game : Game) -> (gameMap : Map) -> (qry : Query result) -> result
query (MkGame _ (MkGameState _ side _ units)) gameMap (SupplyPath unitName) =
  case find ( \ (u, _) => fullName u == unitName) units of
    Nothing => Left (UnitDoesNotExist unitName)
    (Just (unit, pos)) =>
      case supplyPathTo units gameMap (supplySources (nation unit) gameMap) (unit, pos) of
        [] => Left (NoSupplyPathFor unitName pos)
        (x :: _) => Right x
