module Bautzen.Pos

import Data.Nat.DivMod

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
  neighbours (Hex ((S (S _)) + (_ * fromInteger 2)) _)   | (MkDivMod _ (S (S _)) (LTESucc lte)) = [] -- this case is odd...

namespace PosTest
  %access private

  neighbours1_test : (neighbours (Hex 3 3) = [ Hex 2 3, Hex 3 2, Hex 4 3, Hex 4 4, Hex 3 4, Hex 2 4] )
  neighbours1_test = Refl

  neighbours_test : (neighbours (Hex 2 2) = [ Hex 1 1, Hex 2 1, Hex 3 1, Hex 3 2, Hex 2 3, Hex 1 2] )
  neighbours_test = Refl
