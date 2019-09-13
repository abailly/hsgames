module Bautzen.GameUnit

import Data.Fin

%access public export

%default total

-- Base Types
-- Section 1.1

data Nation : Type where
  German : Nation
  Russian : Nation
  Polish : Nation

data Side : Type where
  Axis : Side
  Allies : Side

Eq Side where
  Axis == Axis = True
  Allies == Allies = True
  _ == _ = False

side : Nation -> Side
side German = Axis
side Polish = Allies
side Russian = Allies

friendly : Nation -> Nation -> Bool
friendly n n' = side n == side n'

data UnitType : Type where
  Armored : UnitType
  HeavyArmored : UnitType
  MechInfantry : UnitType
  Infantry : UnitType
  HeavyEngineer : UnitType
  Artillery : UnitType
  AntiTank : UnitType
  HQ : UnitType
  SupplyColumn : UnitType

data UnitSize : Type where
  Regiment : UnitSize
  Brigade : UnitSize
  Division : UnitSize
  Corps : UnitSize

record StdFactors where
  constructor MkStdFactors
  attack : Nat
  defense : Nat

record Arty where
  constructor MkArty
  support : Nat
  distance : Nat

Factors : UnitType -> Type
Factors Armored = StdFactors
Factors HeavyArmored = StdFactors
Factors MechInfantry = StdFactors
Factors Infantry = StdFactors
Factors HeavyEngineer = StdFactors
Factors Artillery = Arty
Factors AntiTank = StdFactors
Factors HQ = Arty
Factors SupplyColumn = ()

record GameUnit where
  constructor MkGameUnit
  nation : Nation
  unitType : UnitType
  name : String
  size : UnitSize
  move : Nat
  currentMP : Nat
  hit : Bool
  combat : Factors unitType

Eq GameUnit where
  unit == unit' = name unit == name unit'

-- list of existing units

-- Russian/Polish

r13_5dp : GameUnit
r13_5dp = MkGameUnit Russian Infantry "13/5DP" Regiment 6 6 False (MkStdFactors 3 4)

-- German

g21_20pz : GameUnit
g21_20pz = MkGameUnit German Armored "21/20Pz" Regiment 10 10 False (MkStdFactors 6 4)
