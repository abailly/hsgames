module Data.Nat.Parity

import Data.Nat

--------------------------------------------------------------------------------
-- Parity
--------------------------------------------------------------------------------

mutual
  public export
  even : Nat -> Bool
  even Z = True
  even (S k) = odd k

  public export
  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

||| A nat is Even when it is twice some other nat.
public export
Even : Nat -> Type
Even n = (half : Nat ** n = half * 2)

||| A nat is Odd when it is one more than twice some other nat.
public export
Odd : Nat -> Type
Odd n = (haf : Nat ** n = S (haf * 2))

||| Two more than an Even is Even.
public export
add2Even : Even n -> Even (2 + n)
add2Even (half ** pf) = (S half ** cong ((+) 2) pf)

||| Two more than an Odd is Odd.
public export
add2Odd : Odd n -> Odd (2 + n)
add2Odd (haf ** pf) = (S haf ** cong ((+) 2) pf)

||| One more than an Even is Odd.
public export
succEvenOdd : Even n -> Odd (S n)
succEvenOdd (half ** pf) = (half ** cong S pf)

||| One more than an Odd is Even.
public export
succOddEven : Odd n -> Even (S n)
succOddEven (haf ** pf) = (S haf ** cong S pf)

||| One less than an Odd is Even.
public export
predOddEven : Odd (S n) -> Even n
predOddEven (haf ** pf) = (haf ** cong Nat.pred pf)

||| A helper fact.
public export
succDoublePredPred : {k : Nat} -> S n = k * 2 -> n = S ((pred k) * 2)
succDoublePredPred {k = Z} prf = absurd prf
succDoublePredPred {k = S _} prf = cong Nat.pred prf

||| One less than an Even is Odd.
public export
predEvenOdd : Even (S n) -> Odd n
predEvenOdd (half ** pf) = (pred half ** succDoublePredPred pf)

||| Every nat is either Even or Odd.
public export
evenOrOdd : (n : Nat) -> Either (Even n) (Odd n)
evenOrOdd Z = Left (0 ** Refl)
evenOrOdd (S k) = case evenOrOdd k of
  Left (half ** pf) => Right (half ** cong S pf)
  Right (haf ** pf) => Left (S haf ** cong S pf)

||| No nat is both Even and Odd.
public export
notEvenAndOdd : {n : Nat} -> Even n -> Odd n -> Void
notEvenAndOdd {n = Z} _ (_ ** odd) = absurd odd
notEvenAndOdd {n = (S k)} (half ** even) (haf ** odd) =
  notEvenAndOdd {n = k}
   (haf ** cong Nat.pred odd)
   (pred half ** succDoublePredPred even)

||| Evenness is decidable.
public export
evenDec : (n : Nat) -> Dec (Even n)
evenDec n = case evenOrOdd n of
  Left even => Yes even
  Right odd => No $ \even => notEvenAndOdd even odd

||| Oddness is decidable.
public export
oddDec : (n : Nat) -> Dec (Odd n)
oddDec n = case evenOrOdd n of
  Left even => No $ notEvenAndOdd even
  Right odd => Yes odd

mutual
  ||| Evens are even.
  public export
  evenEven : {n : Nat} -> Even n -> even n = True
  evenEven {n = Z} _ = Refl
  evenEven {n = S _} pf = oddOdd $ predEvenOdd pf

  ||| Odds are odd.
  public export
  oddOdd : {n: Nat} -> Odd n -> odd n = True
  oddOdd {n = Z} (_ ** pf) = absurd pf
  oddOdd {n = S _} pf = evenEven $ predOddEven pf
