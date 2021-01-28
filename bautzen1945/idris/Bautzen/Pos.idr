||| Position and geometry of Hexagonal grid system
||| https://www.redblobgames.com/grids/hexagons/
module Bautzen.Pos

import Data.Fin
import Data.Nat
import Data.Nat.DivMod
import Data.Nat.Parity
import Decidable.Equality

import public Data.ZZ
import public Data.ZZ.Extra
import Data.Maybe.Extra


%default total

-- Positions & Map

||| A position/hex of the game board encoded as a pair of `Fin`s.
||| The type is indexed by the bounds on the number of rows and cols
||| which are basically dependent on the size of the terrain.
public export
data Pos : (c : Nat) -> (r : Nat) -> Type where
  Hex : (col : Fin c) -> (row : Fin r) -> Pos c r

colInjective : Hex c r = Hex col r' -> c = col
colInjective Refl = Refl

rowInjective : Hex c r = Hex c' row -> r = row
rowInjective Refl = Refl

public export
Eq (Pos c r) where
  (==) (Hex col row) (Hex col' row') = col == col' && row == row'

public export
DecEq (Pos c r) where
  decEq (Hex col row) (Hex k j ) with (decEq col k)
    decEq (Hex col row) (Hex k j ) | No contra = No $ \ h => contra (colInjective h)
    decEq (Hex col row) (Hex k j ) | Yes p with (decEq row j)
    decEq (Hex col row) (Hex k j ) | Yes p | No contra = No $ \ h => contra (rowInjective h)
    decEq (Hex col row) (Hex col row) | Yes Refl | Yes Refl = Yes Refl

public export
Show (Pos c r) where
  show (Hex c r) = show2Digits (finToNat c) ++ show2Digits (finToNat r)
    where
      show2Digits : Nat -> String
      show2Digits n =
        if n < 9
        then "0" ++ show (n + 1)
        else show (n + 1)

public export
Ord (Pos c r) where
  compare (Hex col row) (Hex col' row') =
    case compare col col' of
      LT => LT
      EQ => compare row row'
      GT => GT

||| Cube coordinates.
||| Cube coordinates stem from the observation a 2-D hexagonal grid is equivalent
||| to a diagonal "slice" of a 3-D cubic grid. Using cubic coordinates makes it
||| much easier to compute geometric values.
||| We only store the `x` (column) and `z` (depth) coordinates instead
||| of a triple as the `y` dimension can be simply recovered as `-x -z`.
public export
data Cube : Type where
  MkCube : (x : ZZ) -> (z : ZZ) -> Cube


||| Compute the L1 distance between 2 `Cube`s
||| see [Red Blob Games](https://www.redblobgames.com/grids/hexagons/#distances-cube) page
||| for details on the (pretty cool) algorithm.
public export
cubeDistance : Cube -> Cube -> Nat
cubeDistance (MkCube x z) (MkCube x' z') =
  let y  = negate x - z
      y' = negate x' - z'
  in max (max (absZ (x - x')) (absZ (y - y'))) (absZ (z - z'))

public export
posToCube : Pos c r -> Cube
posToCube (Hex col row) =
  let x = Pos (finToNat col)
      sign : ZZ
      sign = if odd (finToNat col) then 1 else 0
      z : ZZ
      z = Pos (finToNat row) - divZZNZ (x - sign) 2 {z = PosSIsNotZ }
  in MkCube x z

public export
distance : {c : Nat} -> {r : Nat} -> Pos c r -> Pos c r -> Nat
distance x y = cubeDistance (posToCube x) (posToCube y)

public export
data Mvmt : Type where
  Dec : Mvmt
  Neut : Mvmt
  Inc : Mvmt


public export
succNotLTEZ : Not (LTE (S m) Z)
succNotLTEZ LTEZero impossible

public export
fromLTESucc : LTE (S m) (S n) -> LTE m n
fromLTESucc (LTESucc x) = x

public export
lteSuccR : LTE n m -> LTE n (S m)
lteSuccR LTEZero     = LTEZero
lteSuccR (LTESucc x) = LTESucc (lteSuccR x)

public export
lteSuccL : LTE (S n) m -> LTE n m
lteSuccL (LTESucc x) = lteSuccR x

public export
isLte : (m, n : Nat) -> Dec (LTE m n)
isLte Z n = Yes LTEZero
isLte (S k) Z = No succNotLTEZ
isLte (S k) (S j)
    = case isLte k j of
           No contra => No (contra . fromLTESucc)
           Yes prf => Yes (LTESucc prf)


public export
shiftPos : {bound : Nat} -> (x : Nat) -> Mvmt -> Maybe (Fin bound)
shiftPos Z Dec = Nothing
shiftPos (S k) Dec = natToFin k bound
shiftPos x Neut = natToFin x bound
shiftPos x Inc {bound} = natToFin (S x) bound


public export
makePos : {c : Nat} -> {r : Nat} -> (x : Nat) -> (y : Nat) -> (Mvmt, Mvmt) -> Maybe (Pos c r)
makePos col row (a, b) = do
  c' <- shiftPos col a
  r' <- shiftPos row b
  pure $ Hex c' r'


public export
oddShifts : List (Mvmt, Mvmt)
oddShifts = [ (Dec, Neut)
            , (Neut, Dec)
            , (Inc, Neut)
            , (Inc, Inc)
            , (Neut, Inc)
            , (Dec, Inc)
            ]


public export
evenShifts : List (Mvmt, Mvmt)
evenShifts = [ (Dec, Dec)
            , (Neut, Dec)
            , (Inc, Dec)
            , (Inc, Neut)
            , (Neut, Inc)
            , (Dec, Neut)
            ]

neighbours' : {col : Nat} -> {row : Nat} -> (x : Nat) -> (y : Nat) -> List (Pos col row)
neighbours' x y with (x `divMod` (S Z))
  neighbours' x@(Z + (q * (S(S Z)))) y     | (MkDivMod q Z remainderSmall) = catMaybes $ map (makePos x y) evenShifts
  neighbours' x@((S Z) + (q * (S(S Z)))) y | (MkDivMod q (S Z) remainderSmall) = catMaybes $ map (makePos x y) oddShifts
  neighbours' ((S (S r)) + (q * (S(S Z)))) _   | (MkDivMod q (S (S r)) LTEZero) impossible
  neighbours' ((S (S r)) + (q * (S(S Z)))) _   | (MkDivMod q (S (S r)) (LTESucc lte)) = absurd $ succNotLTEZ (fromLTESucc lte)

||| Compute the neighbours of a given position
||| There are at most 6 neighbours, with side and corner hexes having of
||| course less.
public export
neighbours : {c : Nat} -> {r : Nat} -> (pos : Pos c r) -> List (Pos c r)
neighbours (Hex col row) = neighbours' (finToNat col) (finToNat row)

namespace PosTest

  neighbours1_test : (neighbours {c=22} {r=12} (Hex 3 3) = [ Hex 2 3, Hex 3 2, Hex 4 3, Hex 4 4, Hex 3 4, Hex 2 4] )
  neighbours1_test = Refl

  neighbours_test : (neighbours {c=22} {r=12} (Hex 2 2) = [ Hex 1 1, Hex 2 1, Hex 3 1, Hex 3 2, Hex 2 3, Hex 1 2] )
  neighbours_test = Refl


  distance_to_odd_neighbours_is_1 : map (distance (Hex 3 2)) (neighbours {c=22} {r=12} (Hex 3 2)) = [ 1, 1, 1, 1, 1, 1 ]
  distance_to_odd_neighbours_is_1 = Refl

  distance_to_even_neighbours_is_1 : map (distance (Hex 2 2)) (neighbours {c=22} {r=12} (Hex 2 2)) = [ 1, 1, 1, 1, 1, 1 ]
  distance_to_even_neighbours_is_1 = Refl

  distance_test : distance {c=22} {r=12} (Hex 3 2) (Hex 4 4) = 2
  distance_test = Refl
