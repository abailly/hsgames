module Data.Positive

import Decidable.Order
import public Decidable.Equality

%access public export
%default total


-- Auxiliary lemmas for proving properties of Positive

||| Adding a `Nat` number to the right of an inequation preserves the ordering
plusRightIsLte : LTE j k -> LTE j (k + n)
plusRightIsLte x   {k} = lteTransitive x (lteAddRight k)

||| A proof that multiplying a number by any positive number yields a
||| greater or equal number.
lteMultRight : (n : Nat) -> { auto nz : LTE 1 k } -> LTE n (mult n k)
lteMultRight Z = LTEZero
lteMultRight (S j) {k = Z} impossible
lteMultRight (S j) {k = (S k)} =
  rewrite plusCommutative k (j * S k)
  in LTESucc (plusRightIsLte $ lteMultRight j {k = S k})


||| A proof that the product of 2 numbers greater than 1 is greater than 1
lteOneMult : (nz : LTE 1 n) -> (nz' : LTE 1 k) -> LTE 1 (n * k)
lteOneMult nz nz' {n} = lteTransitive nz (lteMultRight n)

||| An `Positive` number is a strictly positive `Nat`.
data Positive : Type where
  MkPositive : (n : Nat) -> { auto notZero : LTE 1 n } -> Positive

Eq Positive where
  (MkPositive n) == (MkPositive n') = n == n'

lteUniqueProof : (p, q : LTE a b) -> p = q
lteUniqueProof LTEZero LTEZero = Refl
lteUniqueProof (LTESucc x) (LTESucc y) =
  rewrite lteUniqueProof x y in
  Refl

private
eqNIsEqPositive : (prf : n = k)
      -> { notZeron : LTE 1 n }
      -> { notZerok : LTE 1 k }
      -> (MkPositive n = MkPositive k)
eqNIsEqPositive Refl {notZeron} {notZerok} =
  rewrite lteUniqueProof notZeron notZerok in Refl

nInj : (MkPositive n {notZero} = MkPositive m {notZero=notZerom}) -> (n = m)
nInj Refl = Refl

DecEq Positive where
  decEq (MkPositive n {notZero=notZeron}) (MkPositive k {notZero=notZerok}) with (decEq n k)
    | (Yes prf)   = Yes (eqNIsEqPositive prf)
    | (No contra) = No (contra . nInj)

Show Positive where
  show (MkPositive n) = show n

||| Convert an `Integer` to a `Positive`.
||| Negative or null values are mapped to 1
fromIntegerPositive : Integer -> Positive
fromIntegerPositive x =
    let n = fromInteger x
    in case isLTE 1 n of
      (Yes prf)   => MkPositive n
      (No contra) => MkPositive (S Z)

||| Implementation of numeric operations over a Positive number.
|||
||| This is basically lifting the operations from `Nat` but we need to
||| build a proof the result is greater than 0, which makes things  trickier
Num Positive where
  (MkPositive n {notZero=nz}) + (MkPositive k {notZero=nz'}) = MkPositive (n + k) { notZero = plusRightIsLte nz }

  (MkPositive n {notZero=nz}) * (MkPositive k {notZero=nz'}) = MkPositive (n * k) { notZero = lteOneMult nz nz' }

  fromInteger = fromIntegerPositive
