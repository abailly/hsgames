module Bautzen.Pos

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

||| Compute the neighbours of a given position
||| There are at most 6 neighbours, with side and corner hexes having of
||| course less.
neighbours : (pos : Pos) -> List Pos
neighbours pos =
  catMaybes $ map (makePos pos) [ (Inc, Inc)
                                , (Inc, Neut)
                                , (Neut, Inc)
                                , (Neut, Dec)
                                , (Dec, Neut)
                                , (Inc, Dec)
                                ]
