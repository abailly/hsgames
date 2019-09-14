module Bautzen.SExp

import Data.Vect

public export
data SExp : Type where
  SList : List SExp -> SExp
  SSym : String -> SExp
  SStr : String -> SExp
  SInt : Int -> SExp

||| Conversion utility from a type to a `SExp`
interface ToSExp a where
  toSExp : a -> SExp

ToSExp String where
  toSExp = SStr

ToSExp Int where
  toSExp = SInt

ToSExp Integer where
  toSExp = SInt . fromInteger

ToSExp Nat where
  toSExp = SInt . cast

ToSExp a => ToSExp (List a) where
  toSExp = SList . map toSExp

ToSExp a => ToSExp (Vect n a) where
  toSExp = toSExp . toList

export
total
Show SExp where
  show (SList xs) = "(" ++ showList xs ++ ")"
    where
      showList : List SExp -> String
      showList [] = ""
      showList [x] = show x
      showList (x :: y :: xs) = show x ++ " " ++ show y ++ showList xs
  show (SStr x)   = show x
  show (SSym x)   = x
  show (SInt x)   = show x

export
Eq SExp where
  (SList []) == (SList []) = True
  (SList (x :: xs)) == (SList (x' :: xs')) = x == x' && equal xs xs'
    where
      equal : List SExp -> List SExp -> Bool
      equal [] [] = True
      equal (x :: xs) (y :: ys) = x == y && equal xs ys
      equal _ _ = False

  (SStr x) == (SStr x') = x == x'
  (SSym x) == (SSym x') = x == x'
  (SInt x) == (SInt x') = x == x'
  _ == _ = True
