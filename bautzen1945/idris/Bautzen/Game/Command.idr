||| Commands Rules
||| Section 6
module Bautzen.Game.Command

import public Bautzen.GameUnit
import Bautzen.Game.Core
import Bautzen.Pos

import Data.Maybe.Extra

import Data.Fin
import Data.List

atCommandDistance : Maybe Pos -> (Pos, Nat) -> Bool
atCommandDistance Nothing y = False
atCommandDistance (Just posUnit) (posHQ, commandDistance) =
  distance posUnit posHQ <= commandDistance

withinCommandDistance : (unit : GameUnit) -> (hqs : List GameUnit) -> (pos : List (GameUnit, Pos)) -> Bool
withinCommandDistance unit hqs pos =
  any (atCommandDistance (lookup unit pos)) (catMaybes $ map positionAndDistance hqs)
  where
    positionAndDistance : GameUnit -> Maybe (Pos, Nat)
    positionAndDistance unit =
      case unitType unit of
        HQ => (\ p => (p, supportDistance unit)) <$> lookup unit pos
        _ => Nothing

||| Find the HQ for a given formation
findHQ : GameUnit -> List (GameUnit, Pos) -> List GameUnit
findHQ unit units = map fst $ filter (flip isHQFor unit . fst) units


||| Check given `unit` is under command of its HQ or a corps/army HQ
underCommand : (units : List (GameUnit, Pos)) -> (unit : GameUnit) -> Bool
underCommand units unit =
  withinCommandDistance unit hqs units
  where
    hqs : List GameUnit
    hqs = findHQ unit units

namespace CommandTest


  positions : List (GameUnit, Pos)
  positions = [ (Bautzen.GameUnit.p15_5dp, Hex 2 2)
              , (Bautzen.GameUnit.r857_294, Hex 2 3)
              , (Bautzen.GameUnit.p13_5dp, Hex 3 4)
              , (Bautzen.GameUnit.p5dp, Hex 3 6)
              , (Bautzen.GameUnit.p2awp, Hex 4 6)
              ]

  unit_is_under_command_if_with_command_distance_of_own_hq :
    underCommand CommandTest.positions GameUnit.p13_5dp = True
  unit_is_under_command_if_with_command_distance_of_own_hq = Refl

  unit_is_under_command_if_with_command_distance_of_army_or_corps_hq :
    underCommand CommandTest.positions GameUnit.p15_5dp = True
  unit_is_under_command_if_with_command_distance_of_army_or_corps_hq = Refl

  unit_is_not_under_command_of_hq_with_different_nationality :
    underCommand CommandTest.positions GameUnit.r857_294 = False
  unit_is_not_under_command_of_hq_with_different_nationality = Refl
