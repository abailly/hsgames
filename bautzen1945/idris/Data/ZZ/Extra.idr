||| More functions and properties for relative numbers
||| Re-exports all of `Data.ZZ` from contrib for convenience
module Data.ZZ.Extra

import public Data.Nat
import public Data.Sign
import public Data.ZZ

%default total

||| Proof that if the absolute value of a number is 0, then this number is 0
||| @y: a relative number which we take absolute value of
public export
absZIsPosZ : {y : ZZ } -> (absZ y = 0) -> (y = Pos 0)
absZIsPosZ {y = (Pos Z)}     Refl = Refl
absZIsPosZ {y = (Pos (S _))} Refl impossible
absZIsPosZ {y = (NegS _)}    Refl impossible

public export
contrapositive : (a -> b) -> (Not b -> Not a)
contrapositive = flip (.)

||| Proof that if a number is not 0, its absolute value is not 0
||| This is the contrapositive proof to `absZIsPosZ`
||| @y: a Relative number which is not 0
public export
notPosZIsNotAbsZ : {y : ZZ } -> ((y = Pos 0) -> Void) -> ((absZ y = 0) -> Void)
notPosZIsNotAbsZ = contrapositive absZIsPosZ

public export
divNatNZ' : Nat -> (y: Nat) -> { prf : Not (y = Z) } -> Nat
divNatNZ' left Z         {prf} = void (prf Refl)
divNatNZ' left (S right) = div' left left right
  where
    div' : Nat -> Nat -> Nat -> Nat
    div' Z        centre right = Z
    div' (S left) centre right =
      if lte centre right then
        Z
      else
        S (div' left (minus centre (S right)) right)


||| Euclidean division of 2 `ZZ` numbers ensuring divisor is not 0
||| We compute using `Nat`'s division and checking the `multiply` of the signs
||| of both numbers, observing that if the sign is  `Zero` then this implis `x`
||| is 0 and thus the result is 0.
|||
||| @x: dividend of euclidean division
||| @y: divisor of euclidean division
||| @divisorNotZ: a proof the divisor is not null
public export
divZZNZ : (x : ZZ) -> (y : ZZ) -> {z : Not (y = Pos Z) } -> ZZ
divZZNZ x (Pos Z) {z} = absurd (z Refl)
divZZNZ x y {z} with (sign x `multiply` sign y)
  divZZNZ x y {z} | Zero = Pos Z
  divZZNZ x y {z} | Plus = Pos $ divNatNZ' (absZ x) (absZ y) { prf = notPosZIsNotAbsZ z }
  divZZNZ x y {z} | Minus = NegS $ divNatNZ' (absZ x) (absZ y) { prf = notPosZIsNotAbsZ  z }

||| A strictly Positive `ZZ` is not 0
public export
PosSIsNotZ : Not (Pos (S k) = Pos 0)
PosSIsNotZ Refl impossible

||| A negative `ZZ` is not 0
public export
NegSIsNotZ : Not (NegS (S k) = Pos 0)
NegSIsNotZ Refl impossible
