module Bautzen.ZoC

import Bautzen.GameUnit
import Bautzen.Pos

%access public export

-- section 3
-- zones of control

data ZoC : Type where
  InZoC : (side : Side) -> ZoC
  Free : ZoC

Eq ZoC where
  Free == Free = True
  (InZoC s) == (InZoC s') = s == s'
  _ == _ = False

||| Test if given position for given `side` is in the ZoC of the unit.
inZoCOf : (pos : Pos) -> (side : Side) -> (GameUnit, Pos) -> Bool
inZoCOf pos curSide (unit, location) with (curSide == side (nation unit))
  | False = pos `elem` neighbours location
  | True = False

||| Is the given `Pos`ition in an enemy ZoC?
||| This assumes the current `side` is playing and checking ZoCs
inZoC : Side -> List (GameUnit, Pos) -> Pos -> ZoC
inZoC curSide units pos =
  case find (inZoCOf pos curSide) units of
    Nothing => Free
    (Just (unit, _)) => InZoC (side $ nation unit)

-- ZoC tests

inZoCTrue : (inZoCOf (Hex 3 3) Axis (Bautzen.GameUnit.p13_5dp, Hex 3 4) = True)
inZoCTrue = Refl

inZoCTrue2 : (inZoCOf (Hex 4 3) Axis (Bautzen.GameUnit.p13_5dp, Hex 3 4) = False)
inZoCTrue2 = Refl

inZoCFalsePolish : (inZoCOf (Hex 3 3) Allies (Bautzen.GameUnit.p13_5dp, Hex 3 4) = False)
inZoCFalsePolish = Refl
