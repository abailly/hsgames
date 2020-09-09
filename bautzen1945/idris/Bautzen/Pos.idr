||| Position and geometry of Hexagonal grid system
||| https://www.redblobgames.com/grids/hexagons/
module Bautzen.Pos

import Data.Nat
import Data.Nat.DivMod
import Data.Nat.Parity
import Data.ZZ

import public Data.ZZ.Extra
import Data.Maybe.Extra

-- Positions & Map

||| A position/hex of the game board encoded as a pair of `Nat`
||| with bounds
public export
data Pos : Type where
  Hex : (col : Nat) -> (row : Nat)
      -> { auto cbound : LTE col 22 }
      -> { auto rbound : LTE row 12 }
      -> Pos

public export
Eq Pos where
  (==) (Hex col row) (Hex col' row') = col == col' && row == row'

public export
Show Pos where
  show (Hex c r) = show2Digits c ++ show2Digits r
    where
      show2Digits : Nat -> String
      show2Digits n =
        if n < 9
        then "0" ++ show (n + 1)
        else show (n + 1)

public export
Ord Pos where
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
posToCube : Pos -> Cube
posToCube (Hex col row) =
  let x = cast col
      sign = if odd col then 1 else 0
      z = cast row - divZZNZ (x - sign) 2 {z = PosSIsNotZ }
  in MkCube x z

public export
distance : Pos -> Pos -> Nat
distance x y =
  let c1 = posToCube x
      c2 = posToCube y
  in cubeDistance c1 c2

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
shiftPos : (x : Nat) -> {bound : Nat} -> (prf : LTE x bound) -> Mvmt -> Maybe (n : Nat ** LTE n bound)
shiftPos Z prf Dec = Nothing
shiftPos (S k) prf Dec = Just (k ** lteSuccL prf)
shiftPos x prf Neut = Just (x ** prf)
shiftPos x prf Inc {bound} with (isLte (S x) bound)
  shiftPos x prf Inc | (Yes y) = Just (S x ** y)
  shiftPos x prf Inc | (No contra) = Nothing


public export
makePos : (pos : Pos) -> (Mvmt, Mvmt) -> Maybe Pos
makePos (Hex col row {cbound} {rbound} ) (a, b) = do
  (c' ** p1) <- shiftPos col cbound a
  (r' ** p2) <- shiftPos row rbound b
  pure $ Hex c' r' {cbound = p1} {rbound = p2}


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

||| Compute the neighbours of a given position
||| There are at most 6 neighbours, with side and corner hexes having of
||| course less.
public export
neighbours : (pos : Pos) -> List Pos
neighbours (Hex col row) with (col `divMod` (S Z))
  neighbours (Hex col@(Z + (q * (S(S Z)))) row)     | (MkDivMod q Z remainderSmall) = catMaybes $ map (makePos (Hex col row)) evenShifts
  neighbours (Hex col@((S Z) + (q * (S(S Z)))) row) | (MkDivMod q (S Z) remainderSmall) = catMaybes $ map (makePos (Hex col row)) oddShifts
  neighbours (Hex ((S (S r)) + (q * (S(S Z)))) _)   | (MkDivMod q (S (S r)) LTEZero) impossible
  neighbours (Hex ((S (S r)) + (q * (S(S Z)))) _)   | (MkDivMod q (S (S r)) (LTESucc lte)) = absurd $ succNotLTEZ (fromLTESucc lte)

namespace PosTest

  neighbours1_test : (neighbours (Hex 3 3) = [ Hex 2 3, Hex 3 2, Hex 4 3, Hex 4 4, Hex 3 4, Hex 2 4] )
  neighbours1_test = Refl

  neighbours_test : (neighbours (Hex 2 2) = [ Hex 1 1, Hex 2 1, Hex 3 1, Hex 3 2, Hex 2 3, Hex 1 2] )
  neighbours_test = Refl

  -- distance_to_odd_neighbours_is_1 : map (distance (Hex 3 2)) (neighbours (Hex 3 2)) = [ 1, 1, 1, 1, 1, 1 ]
  -- distance_to_odd_neighbours_is_1 = Refl

  -- distance_to_even_neighbours_is_1 : map (distance (Hex 2 2)) (neighbours (Hex 2 2)) = [ 1, 1, 1, 1, 1, 1 ]
  -- distance_to_even_neighbours_is_1 = Refl

  -- distance_test : distance (Hex 3 2) (Hex 4 4) = 2
  -- distance_test = Refl
