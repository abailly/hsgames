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

flipSide : Side -> Side
flipSide Axis = Allies
flipSide Allies = Axis

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

Eq UnitType where
  Armored == Armored = True
  HeavyArmored == HeavyArmored = True
  MechInfantry == MechInfantry = True
  Infantry == Infantry = True
  HeavyEngineer == HeavyEngineer = True
  Artillery == Artillery = True
  AntiTank == AntiTank = True
  HQ == HQ = True
  SupplyColumn == SupplyColumn = True
  _ == _ = False

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

record Pak where
  constructor MkPak
  antitank : Nat

Show Pak where
  show (MkPak at) = "Antitank factor=" ++ show at

Factors : UnitType -> Type
Factors Armored       = StdFactors
Factors HeavyArmored  = StdFactors
Factors MechInfantry  = StdFactors
Factors Infantry      = StdFactors
Factors HeavyEngineer = StdFactors
Factors Artillery     = Arty
Factors AntiTank      = Pak
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

isHQ : GameUnit -> Bool
isHQ unit = unitType unit == HQ

||| Check if given `unit` is a HQ for the given formation name
||| To provide valid command for a unit, another unit:
|||
||| * be a `HQ` type of unit
||| * be part of the same formation than the given unit
||| * _or_ be an army/group HQ
|||
||| @hq the HQ we test command for
||| @unit the possibly commanded unit
isHQFor : (hq : GameUnit) -> (unit : GameUnit) -> Bool
isHQFor (MkGameUnit nationality HQ _ Nothing Army _ _ _ _) unit = nation unit == nationality
isHQFor (MkGameUnit _ HQ _ formation _ _ _ _ _) unit = parent unit == formation
isHQFor _ _ = False

||| Attack factor of a `unit` when directly involved
||| in a combat
attackCapacity : (unit : GameUnit) -> Nat
attackCapacity (MkGameUnit _ Armored  _ _ _ _ _ _ ( MkStdFactors attack _ )) = attack
attackCapacity (MkGameUnit _ HeavyArmored  _ _ _ _ _ _ ( MkStdFactors attack _ )) = attack
attackCapacity (MkGameUnit _ MechInfantry  _ _ _ _ _ _ ( MkStdFactors attack _ )) = attack
attackCapacity (MkGameUnit _ Infantry  _ _ _ _ _ _ ( MkStdFactors attack _ )) = attack
attackCapacity (MkGameUnit _ HeavyEngineer  _ _ _ _ _ _ ( MkStdFactors attack _ )) = attack
-- all other units have a raw attack factor of 0 because they can't attack alone
attackCapacity _ = 0

||| Defense factor of a `unit` when directly involved
||| in a combat
defenseCapacity : (unit : GameUnit) -> Nat
defenseCapacity (MkGameUnit _ Armored _ _ _ _ _ _ (MkStdFactors _ defense)) = defense
defenseCapacity (MkGameUnit _ HeavyArmored _ _ _ _ _ _ (MkStdFactors _ defense)) = defense
defenseCapacity (MkGameUnit _ MechInfantry _ _ _ _ _ _ (MkStdFactors _ defense)) = defense
defenseCapacity (MkGameUnit _ Infantry _ _ _ _ _ _ (MkStdFactors _ defense)) = defense
defenseCapacity (MkGameUnit _ HeavyEngineer _ _ _ _ _ _ (MkStdFactors _ defense)) = defense
defenseCapacity (MkGameUnit _ Artillery _ _ _ _ _ _ _) = 1
defenseCapacity (MkGameUnit _ AntiTank _ _ _ _ _ _ _) = 1
defenseCapacity (MkGameUnit _ HQ _ _ _ _ _ _ (MkArty Z _)) = 0
defenseCapacity (MkGameUnit _ HQ _ _ _ _ _ _ (MkArty (S k) _)) = divNatNZ (S k) 2 SIsNotZ
defenseCapacity (MkGameUnit _ SupplyColumn _ _ _ _ _ _ _) = 0

||| Is the unit a supporting unit?
isSupportingUnit : (unit : GameUnit) -> Bool
isSupportingUnit unit =
  case unitType unit of
    Artillery => True
    AntiTank => True
    HQ => True
    _ => False

||| Support factor of a unit
supportCapacity : (unit : GameUnit) -> Nat
supportCapacity (MkGameUnit _ Artillery _ _ _ _ _ _  (MkArty support _)) = support
supportCapacity (MkGameUnit _ AntiTank _ _ _ _ _ _  (MkPak antitank)) = antitank
supportCapacity (MkGameUnit _ HQ _ _ _ _ _ _  (MkArty support _)) = support
supportCapacity _ = 0

supportDistance : (unit : GameUnit) -> Nat
supportDistance (MkGameUnit _ Artillery _ _ _ _ _ _  (MkArty _ distance)) = distance
supportDistance (MkGameUnit _ AntiTank _ _ _ _ _ _  (MkPak antitank)) = 0
supportDistance (MkGameUnit _ HQ _ _ _ _ _ _  (MkArty _ distance)) = distance
supportDistance _ = 0

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

g59_20pz : GameUnit
g59_20pz = MkGameUnit German MechInfantry "59" (Just "20Pz") Regiment 10 10 False (MkStdFactors 4 5)

g20pz : GameUnit
g20pz = MkGameUnit German HQ "HQ" (Just "20Pz") Division 8 8 False (MkArty 6 3)

gBrgPzG : GameUnit
gBrgPzG = MkGameUnit German HQ "HQ" (Just "BrgPzG") Division 8 8 False (MkArty 7 3)

g777Arty : GameUnit
g777Arty = MkGameUnit German Artillery "777" Nothing Brigade 5 5 False (MkArty 6 4)

gSupplyColumn : GameUnit
gSupplyColumn = MkGameUnit German SupplyColumn "SC" Nothing Brigade 4 4 False ()
