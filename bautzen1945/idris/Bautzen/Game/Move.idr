module Bautzen.Game.Move

import Bautzen.Game.Core
import Bautzen.GameUnit
import Bautzen.Pos
import Bautzen.Terrain
import Bautzen.ZoC

%access export
%default total

updateMovedUnit : (unit : GameUnit) -> Pos -> (mps : Nat) -> { auto prf : LTE mps (currentMP unit) } -> List (GameUnit, Pos) -> List (GameUnit, Pos)
updateMovedUnit unit newPosition mps = foldr updateUnit []
  where
    updateUnit : (GameUnit, Pos) -> List (GameUnit, Pos) -> List (GameUnit, Pos)
    updateUnit u@(gu, pos) acc =
      if fullName unit == fullName gu
      then let unit' = record { currentMP = currentMP unit - mps } unit
           in (unit', newPosition) :: acc
      else u :: acc

movementCost : (unit : GameUnit) -> (units : List (GameUnit, Pos)) -> (gameMap : Map) -> (from : Pos) -> (to : Pos)
             -> Bool
             -> Either GameError (cost : Cost ** LTE (toNat cost) (currentMP unit))
movementCost unit units gameMap from to leavingZoC with (cost (unitType unit) (terrain to gameMap) (connection from to gameMap))
  movementCost unit units _ from to _     | Impossible = Left (ForbiddenTerrain from to)
  movementCost unit units _ from to False | c = case isLTE (toNat c) (currentMP unit) of
                                                           Yes prf => Right (c ** prf)
                                                           No _    => Left (NotEnoughMPs unit from to (toNat c))
  movementCost unit units _ from to True  | c = let cost = One c
                                                in case isLTE (toNat cost) (currentMP unit) of
                                                           Yes prf => Right (cost ** prf)
                                                           No _    => Left (NotEnoughMPs unit from to (toNat cost))

moreMoveTo : (unit : GameUnit) -> (units : List (GameUnit, Pos)) -> (gameMap : Map) -> (from : Pos) -> (to : Pos) -> Either GameError Event
moreMoveTo unit units gameMap from to with (inZoC (side (nation unit)) units from, inZoC (side (nation unit)) units to)
  | (InZoC _, Free) = do (c ** _) <- movementCost unit units gameMap from to True
                         pure (Moved unit from to c)
  | (Free, _) = do (c ** _) <- movementCost unit units gameMap from to False
                   pure (Moved unit from to c)
  | (_, _) = Left (MoveFromZocToZoc unit to)

moveTo : (side : Side) -> (units : List (GameUnit, Pos)) -> Map -> (unitName : String) -> (to : Pos) -> Either GameError Event
moveTo sideToPlay units gameMap unitName to =
  case find (\ (u,_) => fullName u == unitName) units of
    Nothing => Left (NoSuchUnits [unitName])
    (Just (unit, b)) => if side (nation unit) /= sideToPlay
                        then Left (NotYourTurn (side (nation unit)))
                        else if not (to `elem` (neighbours b))
                             then Left (InvalidMove b to)
                             else case find (\ (u,p) => p == to) units of
                                       Nothing => moreMoveTo unit units gameMap b to
                                       (Just (other, _)) => if friendly (nation unit) (nation other)
                                                            then moreMoveTo unit units gameMap b to
                                                            else Left (EnemyInHex other to)

namespace MoveTest
  %access private
  cannot_move_if_unit_does_not_exist : moveTo Allies [ (Bautzen.GameUnit.p13_5dp, Hex 3 4) ] TestMap "foo" (Hex 3 5) = Left (NoSuchUnits [ "foo" ])
  cannot_move_if_unit_does_not_exist = Refl

  cannot_move_not_current_side : moveTo Axis [ (Bautzen.GameUnit.p13_5dp, Hex 3 4) ] TestMap "13/5DP" (Hex 3 5) = Left (NotYourTurn Allies)
  cannot_move_not_current_side = Refl

  cannot_move_if_target_hex_is_occupied_by_enemy : moveTo Allies [ (Bautzen.GameUnit.p13_5dp, Hex 3 4), (Bautzen.GameUnit.g21_20pz, Hex 3 5) ] TestMap "13/5DP" (Hex 3 5) = Left (EnemyInHex Bautzen.GameUnit.g21_20pz (Hex 3 5))
  cannot_move_if_target_hex_is_occupied_by_enemy = Refl

  cannot_move_from_zoc_to_zoc : moveTo Allies [ (Bautzen.GameUnit.p13_5dp, Hex 3 4), (Bautzen.GameUnit.g21_20pz, Hex 3 5) ] TestMap "13/5DP" (Hex 4 5) = Left (MoveFromZocToZoc Bautzen.GameUnit.p13_5dp (Hex 4 5))
  cannot_move_from_zoc_to_zoc = Refl

  moving_into_clear_terrain_costs_1 : moveTo Allies [ (Bautzen.GameUnit.p13_5dp, Hex 3 4) ] TestMap "13/5DP" (Hex 4 4) = Right (Moved Bautzen.GameUnit.p13_5dp (Hex 3 4) (Hex 4 4) (One Zero))
  moving_into_clear_terrain_costs_1 = Refl

  infantry_moving_into_rough_terrain_costs_1 : moveTo Allies [ (Bautzen.GameUnit.p13_5dp, Hex 3 4) ] TestMap "13/5DP" (Hex 3 3) = Right (Moved Bautzen.GameUnit.p13_5dp (Hex 3 4) (Hex 3 3) (One Zero))
  infantry_moving_into_rough_terrain_costs_1 = Refl

  non_infantry_moving_into_rough_terrain_costs_2 : moveTo Axis [ (Bautzen.GameUnit.g21_20pz, Hex 3 4) ] TestMap "21/20Pz" (Hex 3 3) = Right (Moved Bautzen.GameUnit.g21_20pz (Hex 3 4) (Hex 3 3) (Two Zero))
  non_infantry_moving_into_rough_terrain_costs_2 = Refl

  cost_for_rough_is_cumulative : moveTo Allies [ (Bautzen.GameUnit.p13_5dp, Hex 3 4) ] TestMap "13/5DP" (Hex 3 5) = Right (Moved Bautzen.GameUnit.p13_5dp (Hex 3 4) (Hex 3 5) (One (One Zero)))
  cost_for_rough_is_cumulative = Refl

  armored_cost_for_rough_is_cumulative : moveTo Axis [ (Bautzen.GameUnit.g21_20pz, Hex 3 4) ] TestMap "21/20Pz" (Hex 3 5) = Right (Moved Bautzen.GameUnit.g21_20pz (Hex 3 4) (Hex 3 5) (Two (Two Zero)))
  armored_cost_for_rough_is_cumulative = Refl

  moving_across_a_lake_is_forbidden : moveTo Axis [ (Bautzen.GameUnit.g21_20pz, Hex 8 6) ] TestMap "21/20Pz" (Hex 8 7) = Left (ForbiddenTerrain (Hex 8 6) (Hex 8 7))
  moving_across_a_lake_is_forbidden = Refl

  armored_moving_on_hill_is_forbidden : moveTo Axis [ (Bautzen.GameUnit.g21_20pz, Hex 3 4) ] TestMap "21/20Pz" (Hex 4 5) = Left (ForbiddenTerrain (Hex 3 4) (Hex 4 5))
  armored_moving_on_hill_is_forbidden = Refl

  armored_moving_on_hill_through_road_costs_half : moveTo Axis [ (Bautzen.GameUnit.g21_20pz, Hex 8 7) ] TestMap "21/20Pz" (Hex 7 7) = Right (Moved Bautzen.GameUnit.g21_20pz (Hex 8 7) (Hex 7 7) (Half (Two Zero)))
  armored_moving_on_hill_through_road_costs_half = Refl

  armored_moving_through_road_costs_half : moveTo Axis [ (Bautzen.GameUnit.g21_20pz, Hex 4 4) ] TestMap "21/20Pz" (Hex 5 4) = Right (Moved Bautzen.GameUnit.g21_20pz (Hex 4 4) (Hex 5 4) (Half (One Zero)))
  armored_moving_through_road_costs_half = Refl

  infantry_moving_through_road_costs_half : moveTo Allies [ (Bautzen.GameUnit.p13_5dp, Hex 8 7) ] TestMap "13/5DP" (Hex 7 7) = Right (Moved Bautzen.GameUnit.p13_5dp (Hex 8 7) (Hex 7 7) (Half (Two (One Zero))))
  infantry_moving_through_road_costs_half = Refl

  river_adds_one_PM_to_move : moveTo Allies [ (Bautzen.GameUnit.p13_5dp, Hex 10 3) ] TestMap "13/5DP" (Hex 10 2) = Right (Moved Bautzen.GameUnit.p13_5dp (Hex 10 3) (Hex 10 2) (Half (One (One (One Zero)))))
  river_adds_one_PM_to_move = Refl

  moving_out_of_ZoC_adds_one_PM_to_move : moveTo Allies [ (Bautzen.GameUnit.p13_5dp, Hex 3 4), (Bautzen.GameUnit.g21_20pz, Hex 3 5) ] TestMap "13/5DP" (Hex 4 4) = Right (Moved Bautzen.GameUnit.p13_5dp (Hex 3 4) (Hex 4 4) (One (One Zero)))
  moving_out_of_ZoC_adds_one_PM_to_move = Refl

  g21_20pz_no_mp : GameUnit
  g21_20pz_no_mp = MkGameUnit German Armored "21" (Just "20Pz") Regiment 10 3 False (MkStdFactors 6 4)

  unit_cannot_move_if_not_enough_MP : moveTo Axis [ (MoveTest.g21_20pz_no_mp, Hex 3 4) ] TestMap "21/20Pz" (Hex 3 5) = Left (NotEnoughMPs MoveTest.g21_20pz_no_mp (Hex 3 4) (Hex 3 5) 4)
  unit_cannot_move_if_not_enough_MP = Refl

  cannot_move_more_than_one_hex : moveTo Axis [ (Bautzen.GameUnit.g21_20pz, Hex 3 4) ] TestMap "21/20Pz" (Hex 3 6) = Left (InvalidMove (Hex 3 4) (Hex 3 6))
  cannot_move_more_than_one_hex = Refl
