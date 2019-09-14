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

Show Nation where
  show German = "German"
  show Russian = "Russian"
  show Polish = "Polish"

data Side : Type where
  Axis : Side
  Allies : Side

Show Side where
  show Axis = "Axis"
  show Allies = "Allies"

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

Show UnitType where
  show Armored = "Armored"
  show HeavyArmored = "HeavyArmored"
  show MechInfantry = "MechInfantry"
  show Infantry = "Infantry"
  show HeavyEngineer = "HeavyEngineer"
  show Artillery = "Artillery"
  show AntiTank = "AntiTank"
  show HQ = "HQ"
  show SupplyColumn = "SupplyColumn"

data UnitSize : Type where
  Regiment : UnitSize
  Brigade : UnitSize
  Division : UnitSize
  Corps : UnitSize

Show UnitSize where
  show Regiment = "Regiment"
  show Brigade = "Brigade"
  show Division = "Division"
  show Corps = "Corps"

record StdFactors where
  constructor MkStdFactors
  attack : Nat
  defense : Nat

Show StdFactors where
  show (MkStdFactors atk def) = "Standard attack=" ++ show atk++", defense=" ++ show def

record Arty where
  constructor MkArty
  support : Nat
  distance : Nat

Show Arty where
  show (MkArty sup dist) = "Arty support=" ++ show sup ++ ", distance=" ++ show dist

Factors : UnitType -> Type
Factors Armored       = StdFactors
Factors HeavyArmored  = StdFactors
Factors MechInfantry  = StdFactors
Factors Infantry      = StdFactors
Factors HeavyEngineer = StdFactors
Factors Artillery     = Arty
Factors AntiTank      = StdFactors
Factors HQ            = Arty
Factors SupplyColumn  = Unit

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

Show GameUnit where
  show (MkGameUnit nation unitType name size move currentMP hit combat) =
    "MkGameUnit nation=" ++ show nation ++
    ", unitType=" ++ show unitType ++
    ", name=" ++ name ++
    ", size=" ++ show size ++
    ", move=" ++ show move ++
    ", currentMP=" ++ show currentMP ++
    ", hit=" ++ show hit ++
    ", combat=" ++ (case unitType of
                         Armored => show combat
                         HeavyArmored => show combat
                         MechInfantry => show combat
                         Infantry => show combat
                         HeavyEngineer => show combat
                         Artillery => show combat
                         AntiTank => show combat
                         HQ => show combat
                         SupplyColumn => show combat)

Eq GameUnit where
  unit == unit' = name unit == name unit'

-- list of existing units

-- Russian/Polish

r13_5dp : GameUnit
r13_5dp = MkGameUnit Russian Infantry "13/5DP" Regiment 6 6 False (MkStdFactors 3 4)

-- German

g21_20pz : GameUnit
g21_20pz = MkGameUnit German Armored "21/20Pz" Regiment 10 10 False (MkStdFactors 6 4)
