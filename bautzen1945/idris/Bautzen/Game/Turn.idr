||| Turns and segment logic
module Bautzen.Game.Turn

import Bautzen.GameUnit
import Bautzen.Game.Core
import Bautzen.Pos
import Bautzen.Terrain

import Data.Fin

%access export
%default total

nextSegment : Game -> Either GameError Event
nextSegment (MkGame _ (MkGameState turn side Supply _) _) = Right (SegmentChanged Supply Move)
nextSegment (MkGame _ (MkGameState turn side Move _) _) = Right (SegmentChanged Move (Combat NoCombat))
nextSegment (MkGame _ (MkGameState turn side (Combat phase) _) _) = ?hole_3
