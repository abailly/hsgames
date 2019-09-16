||| Position and geometry of Hexagonal grid system
||| https://www.redblobgames.com/grids/hexagons/
module Bautzen.Pos

import Data.Nat.DivMod
import Data.Nat.Parity
import Decidable.Order

import public Data.ZZ.Extra

%access public export
%default total

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
  show (Hex c r) = show2Digits c ++ show2Digits r
    where
      show2Digits : Nat -> String
      show2Digits n =
        if n < 9
        then "0" ++ show (n + 1)
        else show (n + 1)

||| Cube coordinates.
||| Cube coordinates stem from the observation a 2-D hexagonal grid is equivalent
||| to a diagonal "slice" of a 3-D cubic grid. Using cubic coordinates makes it
||| much easier to compute geometric values.
||| We only store the `x` (column) and `z` (depth) coordinates instead
||| of a triple as the `y` dimension can be simply recovered as `-x -z`.
data Cube : Type where
  MkCube : (x : ZZ) -> (z : ZZ) -> Cube


||| Compute the L1 distance between 2 `Cube`s
||| see [Red Blob Games](https://www.redblobgames.com/grids/hexagons/#distances-cube) page
||| for details on the (pretty cool) algorithm.
cubeDistance : Cube -> Cube -> Nat
cubeDistance (MkCube x z) (MkCube x' z') =
  let y  = negate x - z
      y' = negate x' - z'
  in max (max (absZ (x - x')) (absZ (y - y'))) (absZ (z - z'))

posToCube : Pos -> Cube
posToCube (Hex col row) =
  let x = cast col
      sign = if odd col then 1 else 0
      z = cast row - divZZNZ (x - sign) 2 PosSIsNotZ
  in MkCube x z

distance : Pos -> Pos -> Nat
distance x y =
  let c1 = posToCube x
      c2 = posToCube y
  in cubeDistance c1 c2

data Mvmt : Type where
  Dec : Mvmt
  Neut : Mvmt
  Inc : Mvmt

shiftPos : (x : Nat) -> (prf : LTE x bound) -> Mvmt -> Maybe (n : Nat ** LTE n bound)
shiftPos Z prf Dec = Nothing
shiftPos (S k) prf Dec = Just (k ** lteSuccLeft prf)
shiftPos x prf Neut = Just (x ** prf)
shiftPos x prf Inc {bound} with (isLTE (S x) bound)
  | (Yes y) = Just (S x ** y)
  | (No contra) = Nothing


makePos : (pos : Pos) -> (Mvmt, Mvmt) -> Maybe Pos
makePos (Hex col row {cbound} {rbound} ) (a, b) = do
  (c' ** p1) <- shiftPos col cbound a
  (r' ** p2) <- shiftPos row rbound b
  pure $ Hex c' r' {cbound = p1} {rbound = p2}


oddShifts : List (Mvmt, Mvmt)
oddShifts = [ (Dec, Neut)
            , (Neut, Dec)
            , (Inc, Neut)
            , (Inc, Inc)
            , (Neut, Inc)
            , (Dec, Inc)
            ]


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
neighbours : (pos : Pos) -> List Pos
neighbours (Hex col row) with (col `divMod` 1)
  neighbours (Hex col@(Z + (_ * fromInteger 2)) row)     | (MkDivMod _ Z remainderSmall) = catMaybes $ map (makePos (Hex col row)) evenShifts
  neighbours (Hex col@((S Z) + (_ * fromInteger 2)) row) | (MkDivMod _ (S Z) remainderSmall) = catMaybes $ map (makePos (Hex col row)) oddShifts
  neighbours (Hex ((S (S _)) + (_ * fromInteger 2)) _)   | (MkDivMod _ (S (S _)) LTEZero) impossible
  neighbours (Hex ((S (S _)) + (_ * fromInteger 2)) _)   | (MkDivMod _ (S (S _)) (LTESucc lte)) = absurd $ succNotLTEzero (fromLteSucc lte)

namespace PosTest
  %access private

  neighbours1_test : (neighbours (Hex 3 3) = [ Hex 2 3, Hex 3 2, Hex 4 3, Hex 4 4, Hex 3 4, Hex 2 4] )
  neighbours1_test = Refl

  neighbours_test : (neighbours (Hex 2 2) = [ Hex 1 1, Hex 2 1, Hex 3 1, Hex 3 2, Hex 2 3, Hex 1 2] )
  neighbours_test = Refl

  distance_to_odd_neighbours_is_1 : map (distance (Hex 3 2)) (neighbours (Hex 3 2)) = [ 1, 1, 1, 1, 1, 1 ]
  distance_to_odd_neighbours_is_1 = Refl

  distance_to_even_neighbours_is_1 : map (distance (Hex 2 2)) (neighbours (Hex 2 2)) = [ 1, 1, 1, 1, 1, 1 ]
  distance_to_even_neighbours_is_1 = Refl

  distance_test : distance (Hex 3 2) (Hex 4 4) = 2
  distance_test = Refl
