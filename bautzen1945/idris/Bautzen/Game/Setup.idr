module Bautzen.Game.Setup

import Bautzen.Game.Core
import Bautzen.GameUnit
import Bautzen.Terrain

import Data.Fin
import Data.List

export
updatePlacedUnit : (unit : GameUnit) -> Pos -> List (GameUnit, Pos) -> List (GameUnit, Pos)
updatePlacedUnit unit newPosition = foldr updateUnit []
  where
    updateUnit : (GameUnit, Pos) -> List (GameUnit, Pos) -> List (GameUnit, Pos)
    updateUnit (gu, pos) acc =
      if fullName unit == fullName gu
      then (unit, newPosition) :: acc
      else (gu, pos) :: acc

public export
placeAt : (side : Side) -> (units : List (GameUnit, Pos)) -> Map -> (unitName : String) -> (pos : Pos) -> Either GameError (Event Setup)
placeAt sideToPlay units map unitName pos =
  case find (sameName unitName) units of
    (Just (unit, b)) => if side (nation unit) /= sideToPlay
                        then Left (NotYourTurn (side (nation unit)))
                        else Left (InvalidPlacement pos)
    Nothing => Left (NoSuchUnits [unitName])


namespace SetupTest

  cannot_place_if_unit_does_not_exist : placeAt Allies [ (Bautzen.GameUnit.p13_5dp, hex 3 4) ] TestMap "foo" (hex 3 5) = Left (NoSuchUnits [ "foo" ])
  cannot_place_if_unit_does_not_exist = Refl

  cannot_place_not_current_side : placeAt Axis [ (Bautzen.GameUnit.p13_5dp, hex 3 4) ] TestMap "13/5DP" (hex 3 5) = Left (NotYourTurn Allies)
  cannot_place_not_current_side = Refl

  cannot_place_if_outside_zone : placeAt Allies [ (Bautzen.GameUnit.p13_5dp, hex 3 4) ] TestMap "13/5DP" (hex 10 4) = Left (InvalidPlacement (hex 10 4))
  cannot_place_if_outside_zone = Refl
