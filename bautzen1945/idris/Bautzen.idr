module Bautzen

import Data.Fin

%default total

-- Base Types
-- Section 1.1

data Nation : Type where
  German : Nation
  Russian : Nation
  Polish : Nation

friendly : Nation -> Nation -> Bool
friendly German German = True
friendly Russian Russian = True
friendly Polish Polish = True
friendly German Russian = False
friendly German Polish = False
friendly Polish German = False
friendly Polish Russian = True
friendly Russian German = False
friendly Russian Polish = True

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
  hit : Bool
  combat : Factors unitType

Eq GameUnit where
  unit == unit' = name unit == name unit'

-- list of existing units

-- Russian/Polish

r13_5dp : GameUnit
r13_5dp = MkGameUnit Russian Infantry "13/5DP" Regiment 6 False (MkStdFactors 3 4)

-- German

g21_20pz : GameUnit
g21_20pz = MkGameUnit German Armored "21/20Pz" Regiment 10 False (MkStdFactors 6 4)

-- Section 2

-- Positions & Map


||| A position/hex of the game board encoded as a pair of `Nat`
||| with bounds
data Pos : Type where
  Hex : (col : Nat) -> (row : Nat)
      -> { auto cbound : LTE col 22 }
      -> { auto rbound : LTE row 12 }
      -> Pos

Eq Pos where
  (==) (Hex col row) (Hex col' row') = col == col' && row == row'

Show Pos where
  show (Hex c r) = show2Digits  c ++ show2Digits r
    where
      show2Digits : Nat -> String
      show2Digits n =
        if n < 9
        then "0" ++ show (n + 1)
        else show (n + 1)

data Mvmt : Type where
  Dec : Mvmt
  Neut : Mvmt
  Inc : Mvmt

||| Compute the neighbours of a given position
||| There are at most 6 neighbours, with side and corner hexes having of
||| course less.

moveTo : (x : Nat) -> (prf : LTE x bound) -> Mvmt -> Maybe (n : Nat ** LTE n bound)
moveTo Z prf Dec = Nothing
moveTo (S k) prf Dec = Just (k ** lteSuccLeft prf)
moveTo x prf Neut = Just (x ** prf)
moveTo x prf Inc {bound} with (isLTE (S x) bound)
  | (Yes y) = Just (S x ** y)
  | (No contra) = Nothing


makePos : (pos : Pos) -> (Mvmt, Mvmt) -> Maybe Pos
makePos (Hex col row {cbound} {rbound} ) (a, b) = do
  (c' ** p1) <- moveTo col cbound a
  (r' ** p2) <- moveTo row rbound b
  pure $ Hex c' r' {cbound = p1} {rbound = p2}

neighbours : (pos : Pos) -> List Pos
neighbours pos =
  catMaybes $ map (makePos pos) [ (Inc, Inc)
                                , (Inc, Neut)
                                , (Neut, Inc)
                                , (Neut, Dec)
                                , (Dec, Neut)
                                , (Dec, Inc)
                                ]

-- Game sequences

data GameSegment : Type where
  Supply : GameSegment
  Move : GameSegment
  Combat : GameSegment

data Event : Type where

record GameState where
  constructor MkGameState
  turn : Fin 5
  side : Nation
  segment : GameSegment
  units : List (GameUnit, Pos)

export
data Game : Type where
  MkGame : List Event -> GameState -> Game

initialState : GameState
initialState = MkGameState 0 German Supply []

export
initialGame : Game
initialGame = MkGame [] initialState


-- section 3
-- zones of control

data ZoC : Type where
  InZoC : (nation : Nation) -> ZoC
  Free : ZoC

inZoCOf : (pos : Pos) -> (side : Nation) -> (GameUnit, Pos) -> Bool
inZoCOf pos side (unit, location) with (friendly side (nation unit))
  | False = pos `elem` neighbours location
  | True = False


||| Is the given `Pos`ition in an enemy ZoC?
||| This assumes the current `side` is playing and  checking ZoCs
inZoC : Pos -> GameState -> ZoC
inZoC pos (MkGameState turn side segment units) =
  case find (inZoCOf pos side) units of
    Nothing => Free
    (Just (MkGameUnit nation _ _ _ _ _ _, _)) => InZoC nation

-- ZoC tests

inZoCTrue : (inZoCOf (Hex 3 3) German (Bautzen.r13_5dp, Hex 3 4) = True)
inZoCTrue = Refl

inZoCFalsePolish : (inZoCOf (Hex 3 3) Polish (Bautzen.r13_5dp, Hex 3 4) = False)
inZoCFalsePolish = Refl

inZoCFalseRussian : (inZoCOf (Hex 3 3) Russian (Bautzen.r13_5dp, Hex 3 4) = False)
inZoCFalseRussian = Refl
