module Bautzen.SExp

import Data.List
import Data.Nat
import Data.Vect
import Control.WellFounded

-- It should be total but it is not due to the mutual recursion in the
-- `toStrings` function
-- %default total

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
ToSExp (Fin n) where
  toSExp = SInt . fromInteger . cast

export
(ToSExp a, ToSExp b) => ToSExp (a, b) where
  toSExp (a, b) = SList [ toSExp a, toSExp b ]

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
(ToSExp a, ToSExp b) => ToSExp (Either a b) where
  toSExp (Left l) = SList [ SSym ":error", toSExp l ]
  toSExp (Right r) = SList [ SSym ":ok", toSExp r ]

export
Show SExp where
  show (SList xs) = "(" ++ showList xs ++ ")"
    where
      showList : List SExp -> String
      showList [] = ""
      showList [x] = show x
      showList (x :: xs) = show x ++ " " ++ showList xs
  show (SStr x)   = show x
  show (SSym x)   = ":" ++ x
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

mutual
  private
  step : (x1 : List SExp) -> ((y : List SExp) -> Smaller y x1 -> Either String (List String)) -> Either String (List String)
  step []        f = Right []
  step (x :: xs) f =
     let lx = (length xs)
         prf = reflexive {rel = LTE}
     in do s <- toStrings x
           ss <- f xs $ LTESucc prf
           pure $ s ++ ss

  ||| Convert a s-expression into a list of strings
  |||
  ||| This function actually _flattens_ the given s-expression, traversing it depth-first and
  ||| expecting to find only _strings_.
  |||
  ||| * A single string is converted as a singleton
  ||| * A list of strings is converted as a list
  ||| * Any other type raises an error
  export
  toStrings : SExp -> Either String (List String)
  toStrings (SStr x) = pure [ x ]
  toStrings (SSym x) = Left $ "Unexpected symbol "++ x ++ ", wanted a string"
  toStrings (SInt x) = Left $ "Unexpected int "++ show x ++ ", wanted a string"
  toStrings (SList x) = sizeRec step x
