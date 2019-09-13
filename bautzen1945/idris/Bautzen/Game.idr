module Bautzen.Game

import public Bautzen.Game.Core
import Bautzen.Game.Move
import public Bautzen.GameUnit
import public Bautzen.Pos
import Bautzen.Terrain

import Data.Fin

%access export

act : (game : Game) -> Command (curSegment game) -> Either GameError Event
act (MkGame events (MkGameState turn side Move units)) (MoveTo unitName to) = moveTo side units TestMap unitName to

apply : Event -> Game -> Game
apply event (MkGame events curState) =
  MkGame (event :: events) (applyEvent event curState)
  where
    applyEvent : Event -> GameState -> GameState
    applyEvent (Moved unit from to cost) (MkGameState turn side segment units) =
      MkGameState turn side segment (updateMovedUnit unit to (toNat cost) units)
