||| Turns and segment logic
module Bautzen.Game.Turn

import Bautzen.Combats
import Bautzen.GameUnit
import Bautzen.Game.Core

import Bautzen.Terrain

import Data.Fin

public export
newTurn : Game -> Either GameError (Event (Combat NoCombat))
newTurn (MkGame (MkGameState FZ _ _ _) _) = Right GameEnded
newTurn (MkGame (MkGameState (FS x) _ _ _) _) = Right $ TurnEnded (weaken x)

public export
nextSegment : (game : Game) -> Either GameError (Event (curSegment game))
nextSegment (MkGame (MkGameState _ _ Supply _) _) = Right (SegmentChanged Supply Move)
nextSegment (MkGame (MkGameState _ _  Move _) _) = Right (SegmentChanged Move (Combat NoCombat))
nextSegment (MkGame (MkGameState _ Axis (Combat NoCombat) _) _) = Right AxisTurnDone
nextSegment game@(MkGame (MkGameState _ Allies (Combat NoCombat) _) _) = newTurn game
nextSegment game@(MkGame (MkGameState _ side seg@(Combat (AssignStrategicSupport supSide state)) _) _) =
  if side == supSide
  then Right (SegmentChanged (Combat (AssignStrategicSupport supSide state)) (Combat (AssignStrategicSupport (flipSide supSide) state)))
  else Right (SegmentChanged (Combat (AssignStrategicSupport supSide state)) (Combat (Resolve state)))
nextSegment (MkGame (MkGameState _ side (Combat _) _) _) = Left $ CombatInProgress side
nextSegment (MkGame (MkGameState _ _ GameEnd _) _) = Left GameHasEnded
