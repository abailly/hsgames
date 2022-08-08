module Bautzen.Game.Setup

import Bautzen.Game.Core
import Bautzen.GameUnit
import Bautzen.Terrain
import Bautzen.Pos

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

UnitSelector : Type
UnitSelector = GameUnit -> Bool

PlacementRule : Type
PlacementRule = Pos -> Bool

findMatch : GameUnit ->  List (UnitSelector, PlacementRule) -> Maybe PlacementRule
findMatch u [] = Nothing
findMatch u ((selector, rule) :: rest) =
  if selector u
  then Just rule
  else findMatch u rest

southOf : Pos -> Fin 13 -> Bool
southOf (MkPos (Hex col x)) row = x <= row

westOf : Pos -> Fin 23 -> Bool
westOf (MkPos (Hex x row)) col = x <= col

infix 8 `southOf`
infix 8 `westOf`

public
export
initialPlacement : List (UnitSelector, PlacementRule)
initialPlacement =
  [  (\ u => u.parent == Just "5DP",
      \ pos => let origin = hex 15 6
               in  distance pos origin <= 1 ),
     (\ u => u.parent == Just "7DP",
      \ pos => let origin = hex 19 2
               in  distance pos origin <= 1 ),
     (\ u => u.parent == Just "10DP",
      \ pos => let origin = hex 18 1
               in  distance pos origin <= 1 ),
     (\ u => u.parent == Just "20Pz",
      \ pos => pos `southOf` 11 || pos `westOf` 3 )
  ]

public export
placeAt : (side : Side) -> (units : List (GameUnit, Pos)) -> Map -> (unitName : String) -> (pos : Pos) -> Either GameError (Event Setup)
placeAt sideToPlay units map unitName pos =
  case find (sameName unitName) units of
    (Just (unit, b)) => if side (nation unit) /= sideToPlay
                        then Left (NotYourTurn (side (nation unit)))
                        else case findMatch unit initialPlacement of
                               Just placementRule => if placementRule pos
                                                     then Right (Placed unit pos)
                                                     else Left (InvalidPlacement pos)
                               Nothing => Left (InvalidPlacement pos)
    Nothing => Left (NoSuchUnits [unitName])


namespace SetupTest

  cannot_place_if_unit_does_not_exist : placeAt Allies [ (Bautzen.GameUnit.p13_5dp, hex 3 4) ] TestMap "foo" (hex 3 5) = Left (NoSuchUnits [ "foo" ])
  cannot_place_if_unit_does_not_exist = Refl

  cannot_place_not_current_side : placeAt Axis [ (Bautzen.GameUnit.p13_5dp, hex 3 4) ] TestMap "13/5DP" (hex 3 5) = Left (NotYourTurn Allies)
  cannot_place_not_current_side = Refl

  -- cannot_place_if_outside_zone : placeAt Allies [ (Bautzen.GameUnit.p13_5dp, hex 3 4) ] TestMap "13/5DP" (hex 10 4) = Left (InvalidPlacement (hex 10 4))
  -- cannot_place_if_outside_zone = Refl

  -- can_place_if_within_zone : placeAt Allies [ (Bautzen.GameUnit.p13_5dp, hex 3 4) ] TestMap "13/5DP" (hex 15 6) = Right (Placed Bautzen.GameUnit.p13_5dp (hex 15 6))
  -- can_place_if_within_zone = Refl
