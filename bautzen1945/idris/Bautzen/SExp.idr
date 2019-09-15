module Bautzen.SExp

import Data.Vect

public export
data SExp : Type where
  SList : List SExp -> SExp
  SSym : String -> SExp
  SStr : String -> SExp
  SInt : Int -> SExp

||| Conversion utility from a type to a `SExp`
public export
interface ToSExp a where
  toSExp : a -> SExp

export
ToSExp SExp where
  toSExp = id

export
ToSExp Unit where
  toSExp () = SList []

export
ToSExp String where
  toSExp = SStr

export
ToSExp Int where
  toSExp = SInt

export
ToSExp Bool where
  toSExp True = SSym "t"
  toSExp False = SSym "nil"

export
ToSExp Integer where
  toSExp = SInt . fromInteger

export
ToSExp Nat where
  toSExp = SInt . cast

export
ToSExp a => ToSExp (Maybe a) where
  toSExp Nothing = SSym "nil"
  toSExp (Just a) = toSExp a

export
ToSExp a => ToSExp (List a) where
  toSExp = SList . map toSExp

export
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
      showList (x :: xs) = show x ++ " " ++ showList xs
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
