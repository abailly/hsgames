||| Euclidean division using primitive recursion/induction.
module Data.Nat.DivMod

import Data.Nat

%default total

||| The result of euclidean division of natural numbers
|||
||| @ dividend the dividend
||| @ divisor the divisor
public export
data DivMod : (dividend, divisor : Nat) -> Type where
  ||| The result of division. The order of operations and operands in the
  ||| result type was picked to not require any explicit proofs.
  |||
  ||| @ quotient the quotient
  ||| @ remainder the remainder
  ||| @ divisor the divisor
  ||| @ remainderSmall proof that the remainder is in the range
  ||| `[0 .. divisor - 1]`
  MkDivMod : (quotient, remainder : Nat) ->
             (remainderSmall : LT remainder divisor) ->
             DivMod (remainder + quotient * divisor) divisor

||| The base case, dividing zero by a successor.
|||
||| @ n the predecessor of the divisor
public export
ZDivModSn : Z `DivMod` S n
ZDivModSn = MkDivMod 0 0 (LTESucc LTEZero)

||| Given a number less than or equal to a bound, is it equal, or less?
|||
||| @ m the lesser or equal number
||| @ n the greater or equal number
||| @ mLten proof that `m` is less than or equal to `n`
public export
stepLT : {n : Nat} -> {m : Nat} -> (mLten : m `LTE` n) -> Either (m = n) (LT m n)
stepLT {m = Z} {n = Z} LTEZero = Left Refl
stepLT {m = Z} {n = (S n)} LTEZero = Right (LTESucc LTEZero)
stepLT {m = (S m)} {n = (S n)} (LTESucc mLten) with (stepLT mLten)
  stepLT {m = (S m)} {n = (S n)} (LTESucc mLten) | (Left mEqn) = Left $ cong S mEqn
  stepLT {m = (S m)} {n = (S n)} (LTESucc mLten) | (Right mLtn) = Right $ LTESucc mLtn

||| The inductive step, taking a division of `m` to a division of `S m`
|||
||| @ m the dividend in the inductive hypothesis
||| @ n the divisor
||| @ hyp the inductive hypothesis
public export
SmDivModn : {n : Nat} -> {m : Nat} -> (hyp : m `DivMod` n) -> (S m) `DivMod` n
SmDivModn {n} {m = r + q * n} (MkDivMod q r rLtn) with (stepLT rLtn)
  SmDivModn {n = S r} {m = r + q * (S r)} (MkDivMod q r rLtn) | (Left Refl) = MkDivMod (S q) 0 (LTESucc LTEZero)
  SmDivModn {n} {m = r + q * n} (MkDivMod q r rLtn) | (Right srLtn) = MkDivMod q (S r) srLtn

||| Euclidean division. Note that this takes the predecessor of the divisor to
||| avoid division by zero.
|||
||| @ dividend the dividend
||| @ predDivisor the predecessor of the desired divisor
public export
divMod : (dividend, predDivisor : Nat) -> dividend `DivMod` (S predDivisor)
divMod Z n = ZDivModSn
divMod (S m) n = SmDivModn (m `divMod` n)
