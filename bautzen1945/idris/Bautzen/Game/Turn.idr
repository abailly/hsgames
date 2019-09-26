||| Turns and segment logic
module Bautzen.Game.Turn

import Bautzen.GameUnit
import Bautzen.Game.Core
import Bautzen.Pos
import Bautzen.Terrain

import Data.Fin

%access export
%default total

newTurn : Game -> Either GameError Event
newTurn (MkGame _ (MkGameState FZ _ _ _) _) = Right GameEnded
newTurn (MkGame _ (MkGameState (FS x) _ _ _) _) = Right $ TurnEnded (weaken x)

nextSegment : Game -> Either GameError Event
nextSegment (MkGame _ (MkGameState _ _ Supply _) _) = Right (SegmentChanged Supply Move)
nextSegment (MkGame _ (MkGameState _ _  Move _) _) = Right (SegmentChanged Move (Combat NoCombat))
nextSegment (MkGame _ (MkGameState _ Axis (Combat NoCombat) _) _) = Right AxisTurnDone
nextSegment game@(MkGame _ (MkGameState _ Allies (Combat NoCombat) _) _) = newTurn game
nextSegment (MkGame _ (MkGameState _ side (Combat _) _) _) = Left $ CombatInProgress side
nextSegment (MkGame _ (MkGameState _ _ GameEnd _) _) = Left GameHasEnded
