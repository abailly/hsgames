||| Rules for stacking
||| Section 5
module Bautzen.Game.Stacking

import Bautzen.GameUnit
import Bautzen.Terrain

%access public export
%default total

-- section 5.1

stackingPoints : Nation -> UnitSize -> UnitType -> Nat
stackingPoints   n         s           SupplyColumn = 1
stackingPoints   n         Regiment    t            = 2
stackingPoints   n         Brigade     t            = 3
stackingPoints   n         Division    HQ           = 3
stackingPoints   Russian   Corps       HQ           = 3
stackingPoints   Russian   Army        HQ           = 3
stackingPoints   n         Corps       Armored      = 3  -- 1KP
stackingPoints   _         _           _            = 1  -- WTF?

-- section 5.2

stackingLimit : Terrain -> Nat
stackingLimit Clear               = 6
stackingLimit Wood                = 4
stackingLimit Rough               = 4
stackingLimit RoughWood           = 4
stackingLimit (Hill base)         = stackingLimit base
stackingLimit (Village base)      = stackingLimit base
stackingLimit Town                = 6
stackingLimit (SupplySource base) = stackingLimit base

-- section 5.4, 5.5, 5.6

data Stacking : Type where
  Stacked : (n : Nat) -> Stacking
  Overstacked : (n : Nat) -> Stacking
  CannotStack : Stacking

||| What's the stacking status of `units` on `terrain`?
||| This can be used to test incrementally the stacking of adding one unit to
||| to an existing stack too.
||| @units a lits of units to check stacking of.
||| @terrain the terrain of the `Hex` the units are stacked on.
stacking : (units : List GameUnit) -> (terrain: Terrain) -> Stacking
stacking units terrain =
  if moreThan2DifferentFormations || unitsFromDifferentNations
  then CannotStack
  else let sp = sum $ map sps units
       in if sp <= stackingLimit terrain
          then Stacked sp
          else Overstacked sp
  where
    unitsFromDifferentNations : Bool
    unitsFromDifferentNations = (length . nub . map nation $ units) > 1

    moreThan2DifferentFormations : Bool
    moreThan2DifferentFormations = (length . catMaybes . nub . map parent $ units) > 2

    sps : GameUnit -> Nat
    sps (MkGameUnit nation unitType name parent size move currentMP hit combat) = stackingPoints nation size unitType

namespace StackingTest
  %access private

  stacking_of_a_single_unit : stacking [ GameUnit.p13_5dp ] Clear = Stacked 2
  stacking_of_a_single_unit = Refl

  stacking_of_more_units_from_same_parent : stacking [ GameUnit.p13_5dp, GameUnit.p15_5dp, GameUnit.p17_5dp ] Clear = Stacked 6
  stacking_of_more_units_from_same_parent = Refl

  overstack_from_units_from_same_parent : stacking [ GameUnit.p13_5dp, GameUnit.p15_5dp, GameUnit.p5dp ] Clear = Overstacked 7
  overstack_from_units_from_same_parent = Refl

  cannot_stack_more_than_1_unit_from_different_parent : stacking [ GameUnit.p13_5dp, GameUnit.p33_7dp, GameUnit.p32_8dp ] Clear = CannotStack
  cannot_stack_more_than_1_unit_from_different_parent = Refl

  can_stack_independent_units : stacking [ GameUnit.p13_5dp, GameUnit.p33_7dp, GameUnit.p6l ] Clear = Overstacked 7
  can_stack_independent_units = Refl

  cannot_stack_units_from_different_nations : stacking [ GameUnit.p13_5dp, GameUnit.r857_294 ] Clear = CannotStack
  cannot_stack_units_from_different_nations = Refl
