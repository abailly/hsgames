module Bautzen.GameUnit

import Data.Fin
import Data.Maybe
import Data.Nat
import Data.Vect
import Decidable.Equality

%default total

-- Base Types
-- Section 1.1
public export
data Nation : Type where
  German : Nation
  Russian : Nation
  Polish : Nation

public export
Eq Nation where
  (==) German German = True
  (==) Russian Russian = True
  (==) Polish Polish = True
  (==) _ _ = False

public export
Show Nation where
  show German = "German"
  show Russian = "Russian"
  show Polish = "Polish"

public export
data Side : Type where
  Axis : Side
  Allies : Side

axisNotAllies : (Axis = Allies) -> Void
axisNotAllies Refl impossible

public export
DecEq Side where
  decEq Axis Axis = Yes Refl
  decEq Allies Allies = Yes Refl
  decEq Axis Allies = No axisNotAllies
  decEq Allies Axis = No $ negEqSym axisNotAllies

public export
Show Side where
  show Axis = "Axis"
  show Allies = "Allies"

public export
Eq Side where
  Axis == Axis = True
  Allies == Allies = True
  _ == _ = False

public export
flipSide : Side -> Side
flipSide Axis = Allies
flipSide Allies = Axis

public export
side : Nation -> Side
side German = Axis
side Polish = Allies
side Russian = Allies

public export
friendly : Nation -> Nation -> Bool
friendly n n' = side n == side n'

public export
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

public export
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

public export
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

public export
data UnitSize : Type where
  Regiment : UnitSize
  Brigade : UnitSize
  Division : UnitSize
  Corps : UnitSize
  Army : UnitSize

public export
Show UnitSize where
  show Regiment = "Regiment"
  show Brigade = "Brigade"
  show Division = "Division"
  show Corps = "Corps"
  show Army = "Army"

public export
record StdFactors where
  constructor MkStdFactors
  attack : Nat
  defense : Nat

public export
Show StdFactors where
  show (MkStdFactors atk def) = "Standard attack=" ++ show atk++", defense=" ++ show def

public export
record Arty where
  constructor MkArty
  support : Nat
  distance : Nat

public export
Show Arty where
  show (MkArty sup dist) = "Arty support=" ++ show sup ++ ", distance=" ++ show dist

public export
record Pak where
  constructor MkPak
  antitank : Nat

public export
Show Pak where
  show (MkPak at) = "Antitank factor=" ++ show at

public export
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

public export
record GameUnit where
  constructor MkGameUnit
  nation : Nation
  unitType : UnitType
  name : String
  parent : Maybe String
  size : UnitSize
  move : Nat
  currentMP : Nat
  steps : Nat
  hits : Fin steps
  combatValue : Vect steps (Factors unitType)

public export
Show GameUnit where
  show (MkGameUnit nation unitType name parent size move currentMP steps hits combat) =
    "MkGameUnit nation=" ++ show nation ++
    ", unitType=" ++ show unitType ++
    ", name=" ++ name ++
    ", parent=" ++ fromMaybe "" parent ++
    ", size=" ++ show size ++
    ", move=" ++ show move ++
    ", currentMP=" ++ show currentMP ++
    ", steps=" ++ show steps ++
    ", hit=" ++ show (finToNat hits) ++
    ", combat=" ++ case unitType of
                     Armored => show combat
                     HeavyArmored => show combat
                     MechInfantry => show combat
                     Infantry => show combat
                     HeavyEngineer => show combat
                     Artillery => show combat
                     AntiTank => show combat
                     HQ => show combat
                     SupplyColumn => show combat

public export
Eq GameUnit where
  unit == unit' = name unit == name unit' && parent unit == parent unit'

public export
fullName : GameUnit -> String
fullName u = name u ++ maybe "" (\ n => "/" ++ n) (parent u)

public export
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
public export
isHQFor : (hq : GameUnit) -> (unit : GameUnit) -> Bool
isHQFor (MkGameUnit nationality HQ _ Nothing Army _ _ _ _ _ ) unit = nation unit == nationality
isHQFor (MkGameUnit _ HQ _ formation _ _ _ _ _ _ ) unit = parent unit == formation
isHQFor _ _ = False

||| Attack factor of a `unit` when directly involved
||| in a combat
public export
attackCapacity : (unit : GameUnit) -> Nat
attackCapacity (MkGameUnit _ Armored  _ _ _ _ _ _ hits combat) = attack (index hits combat)
attackCapacity (MkGameUnit _ HeavyArmored  _ _ _ _ _ _ hits combat) = attack (index hits combat)
attackCapacity (MkGameUnit _ MechInfantry  _ _ _ _ _ _ hits combat) = attack (index hits combat)
attackCapacity (MkGameUnit _ Infantry  _ _ _ _ _ _ hits combat) = attack (index hits combat)
attackCapacity (MkGameUnit _ HeavyEngineer  _ _ _ _ _ _ hits combat) = attack (index hits combat)
-- all other units have a raw attack factor of 0 because they can't attack alone
attackCapacity _ = 0

||| Defense factor of a `unit` when directly involved
||| in a combat
public export
defenseCapacity : (unit : GameUnit) -> Nat
defenseCapacity (MkGameUnit _ Armored _ _ _ _ _ _ hits combat) = defense (index hits combat)
defenseCapacity (MkGameUnit _ HeavyArmored _ _ _ _ _ _ hits combat) = defense (index hits combat)
defenseCapacity (MkGameUnit _ MechInfantry _ _ _ _ _ _ hits combat) = defense (index hits combat)
defenseCapacity (MkGameUnit _ Infantry _ _ _ _ _ _ hits combat) = defense (index hits combat)
defenseCapacity (MkGameUnit _ HeavyEngineer _ _ _ _ _ _ hits combat) = defense (index hits combat)
defenseCapacity (MkGameUnit _ Artillery _ _ _ _ _ _ _ _) = 1
defenseCapacity (MkGameUnit _ AntiTank _ _ _ _ _ _ _ _) = 1
defenseCapacity (MkGameUnit _ HQ _ _ _ _ _ _ hits combat) =
  case index hits combat of
    (MkArty Z distance) => 0
    (MkArty (S k) distance) => divNatNZ (S k) 2 ItIsSucc
defenseCapacity (MkGameUnit _ SupplyColumn _ _ _ _ _ _ _ _) = 0

||| Is the unit a supporting unit?
public export
isSupportingUnit : (unit : GameUnit) -> Bool
isSupportingUnit unit =
  case unitType unit of
    Artillery => True
    AntiTank => True
    HQ => True
    _ => False

||| Support factor of a unit
public export
supportCapacity : (unit : GameUnit) -> Nat
supportCapacity (MkGameUnit _ Artillery _ _ _ _ _ _  hits combat) = support (index hits combat)
supportCapacity (MkGameUnit _ AntiTank _ _ _ _ _ _ hits combat) = antitank (index hits combat)
supportCapacity (MkGameUnit _ HQ _ _ _ _ _ _ hits combat) = support (index hits combat)
supportCapacity _ = 0

public export
supportDistance : (unit : GameUnit) -> Nat
supportDistance (MkGameUnit _ Artillery _ _ _ _ _ _ hits combat) = distance (index hits combat)
supportDistance (MkGameUnit _ AntiTank _ _ _ _ _ _ _ _) = 0
supportDistance (MkGameUnit _ HQ _ _ _ _ _ _ hits combat) = distance (index hits combat)
supportDistance _ = 0

-- -- list of existing units

-- -- Polish

public export
p13_5dp : GameUnit
p13_5dp = MkGameUnit Polish Infantry "13" (Just "5DP") Regiment 6 6 2 0 [ MkStdFactors 3 4, MkStdFactors 1 3 ]


public export
p15_5dp : GameUnit
p15_5dp = MkGameUnit Polish Infantry "15" (Just "5DP") Regiment 6 6 2 0 [ MkStdFactors 3 4, MkStdFactors 1 3 ]

public export
p17_5dp : GameUnit
p17_5dp = MkGameUnit Polish Infantry "17" (Just "5DP") Regiment 6 6 2 0 [ MkStdFactors 3 4, MkStdFactors 1 3 ]

public export
p5dp : GameUnit
p5dp = MkGameUnit Polish HQ "HQ" (Just "5DP") Division 4 4 1 0 [ MkArty 5 2 ]

public export
p33_7dp : GameUnit
p33_7dp = MkGameUnit Polish Infantry "33" (Just "7DP") Regiment 6 6 2 0 [ MkStdFactors 3 4, MkStdFactors 1 3 ]

public export
p32_8dp : GameUnit
p32_8dp = MkGameUnit Polish Infantry "32" (Just "8DP") Regiment 6 6 2 0 [ MkStdFactors 3 4, MkStdFactors 1 3 ]

public export
p6l : GameUnit
p6l = MkGameUnit Polish Artillery "6L" Nothing Brigade 4 4 1 0 [ MkArty 4 4 ]

public export
p2awp : GameUnit
p2awp = MkGameUnit Polish HQ "2AWP" Nothing Army 3 3 1 0 [ MkArty 9 5 ]

-- -- Russian

public export
r857_294 : GameUnit
r857_294 = MkGameUnit Russian Infantry "857" (Just "294") Brigade 8 8 1 0 [ MkStdFactors 2 4 ]

-- -- German

public export
g21_20pz : GameUnit
g21_20pz = MkGameUnit German Armored "21" (Just "20Pz") Regiment 10 10 2 0 [ MkStdFactors 6 4, MkStdFactors 4 3 ]

public export
g59_20pz : GameUnit
g59_20pz = MkGameUnit German MechInfantry "59" (Just "20Pz") Regiment 10 10 2  0 [ MkStdFactors 4 5, MkStdFactors 2 3 ]

public export
g20pz : GameUnit
g20pz = MkGameUnit German HQ "HQ" (Just "20Pz") Division 8 8 1 0 [ MkArty 6 3 ]

public export
gBrgPzG : GameUnit
gBrgPzG = MkGameUnit German HQ "HQ" (Just "BrgPzG") Division 8 8 1 0 [ MkArty 7 3 ]

public export
g777Arty : GameUnit
g777Arty = MkGameUnit German Artillery "777" Nothing Brigade 5 5 1 0 [ MkArty 6 4 ]

public export
gSupplyColumn : GameUnit
gSupplyColumn = MkGameUnit German SupplyColumn "SC" Nothing Brigade 4 4 1 0 [ () ]
