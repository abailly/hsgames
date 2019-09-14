||| Rules for stacking
||| Section 5
module Bautzen.Stacking

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
  Overstack : (n : Nat) -> Stacking
  CannotStack : Stacking

||| What's the stacking status of `units` on `terrain`?
stacking : (units : List GameUnit) -> (terrain: Terrain) -> Stacking
stacking units terrain =
  let sp = sum $ map sps units
  in Stacked sp
  where
    sps : GameUnit -> Nat
    sps (MkGameUnit nation unitType name parent size move currentMP hit combat) = stackingPoints nation size unitType

namespace StackingTest
  %access private

  stacking_of_a_single_unit : stacking [ GameUnit.r13_5dp ] Clear = Stacked 2
  stacking_of_a_single_unit = Refl
