||| Commands Rules
||| Section 6
module Bautzen.Game.Command

import public Bautzen.GameUnit
import Bautzen.Game.Core
import Bautzen.Pos

import Data.Fin

atCommandDistance : Maybe Pos -> (Pos, Nat) -> Bool
atCommandDistance Nothing y = False
atCommandDistance (Just posUnit) (posHQ, commandDistance) =
  distance posUnit posHQ <= commandDistance

withinCommandDistance : (unit : GameUnit) -> (hqs : List GameUnit) -> (pos : List (GameUnit, Pos)) -> Bool
withinCommandDistance unit hqs pos =
  any (atCommandDistance (lookup unit pos)) (catMaybes $ map positionAndDistance hqs)
  where
    positionAndDistance : GameUnit -> Maybe (Pos, Nat)
    positionAndDistance u@(MkGameUnit nation HQ name parent size move currentMP hit (MkArty support distance)) =
      (\ p => (p, distance)) <$> lookup u pos
    positionAndDistance _ = Nothing

||| Find the HQ for a given formation
findHQ : Maybe String -> List (GameUnit, Pos) -> List GameUnit
findHQ Nothing       units = []
findHQ (Just hqName) units = map fst $ filter (isHQFor hqName . fst) units


||| Check given `unit` is under command of its HQ or a corps/army HQ
underCommand : (unit : GameUnit) -> (state : GameState) -> Bool
underCommand unit state =
  withinCommandDistance unit hqs (units state)
  where
    hqs : List GameUnit
    hqs = findHQ (parent unit) (units state)

namespace CommandTest
  %access private

  testGame : GameState
  testGame = MkGameState 0 Axis Move [ (Bautzen.GameUnit.p13_5dp, Hex 3 4)
                                     , (Bautzen.GameUnit.p5dp, Hex 3 6)
                                     ]

  unit_is_under_command_if_with_command_distance_of_own_hq :
    underCommand GameUnit.p13_5dp CommandTest.testGame = True
  unit_is_under_command_if_with_command_distance_of_own_hq = Refl
