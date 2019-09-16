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

Eq Nation where
  (==) German German = True
  (==) Russian Russian = True
  (==) Polish Polish = True
  (==) _ _ = False

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
  Army : UnitSize

Show UnitSize where
  show Regiment = "Regiment"
  show Brigade = "Brigade"
  show Division = "Division"
  show Corps = "Corps"
  show Army = "Army"

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
  parent : Maybe String
  size : UnitSize
  move : Nat
  currentMP : Nat
  hit : Bool
  combat : Factors unitType

Show GameUnit where
  show (MkGameUnit nation unitType name parent size move currentMP hit combat) =
    "MkGameUnit nation=" ++ show nation ++
    ", unitType=" ++ show unitType ++
    ", name=" ++ name ++
    ", parent=" ++ fromMaybe "" parent ++
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
  unit == unit' = name unit == name unit' && parent unit == parent unit'

fullName : GameUnit -> String
fullName u = name u ++ maybe "" (\ n => "/" ++ n) (parent u)

||| Check if given `unit` is a HQ for the given formation name
||| To provide valid command for a unit, another unit:
|||
||| * be a `HQ` type of unit
||| * be part of the same formation than the given unit
||| * _or_ be an army/group HQ
isHQFor : GameUnit -> GameUnit -> Bool
isHQFor unit (MkGameUnit nationality HQ _ Nothing Army _ _ _ _) = nation unit == nationality
isHQFor unit (MkGameUnit _ HQ _ formation _ _ _ _ _) = parent unit == formation
isHQFor _ _ = False

-- list of existing units

-- Polish

p13_5dp : GameUnit
p13_5dp = MkGameUnit Polish Infantry "13" (Just "5DP") Regiment 6 6 False (MkStdFactors 3 4)

p15_5dp : GameUnit
p15_5dp = MkGameUnit Polish Infantry "15" (Just "5DP") Regiment 6 6 False (MkStdFactors 3 4)

p17_5dp : GameUnit
p17_5dp = MkGameUnit Polish Infantry "17" (Just "5DP") Regiment 6 6 False (MkStdFactors 3 4)

p5dp : GameUnit
p5dp = MkGameUnit Polish HQ "HQ" (Just "5DP") Division 4 4 False (MkArty 5 2)

p33_7dp : GameUnit
p33_7dp = MkGameUnit Polish Infantry "33" (Just "7DP") Regiment 6 6 False (MkStdFactors 3 4)

p32_8dp : GameUnit
p32_8dp = MkGameUnit Polish Infantry "32" (Just "8DP") Regiment 6 6 False (MkStdFactors 3 4)

p6l : GameUnit
p6l = MkGameUnit Polish Artillery "6L" Nothing Brigade 4 4 False (MkArty 4 4)

p2awp : GameUnit
p2awp = MkGameUnit Polish HQ "2AWP" Nothing Army 3 3 False (MkArty 9 5)

-- Russian

r857_294 : GameUnit
r857_294 = MkGameUnit Russian Infantry "857" (Just "294") Brigade 8 8 False (MkStdFactors 2 4)

-- German

g21_20pz : GameUnit
g21_20pz = MkGameUnit German Armored "21" (Just "20Pz") Regiment 10 10 False (MkStdFactors 6 4)
