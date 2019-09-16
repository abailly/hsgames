||| Binary representation of natural numbers
||| See:
||| * [Idris tutorial](https://github.com/idris-lang/idris-tutorial/blob/master/examples/binary.idr)
||| * [Haskell](https://jaspervdj.be/posts/2018-09-04-binomial-heaps-101.html#binomial-forests) Binomial heap
|||   making use of binary numbers
module Data.Binary

import public Data.Nat.Parity

||| Right to left representation of binary numbers
||| Working with a right-to-left representation makes addition much easier. We
||| can always reconstruct the usual representation through a `Show` instance
||| for example.
data Binary : (value : Nat) -> Type where
  ||| Denote the end of recursive representation of a number
  BEnd  : Binary Z

  ||| A 0-bit
  ||| Build a binary number equal to 2v
  ||| @bin the rest of this binary number
  ||| @v the value of the rest of this binary number
  B0    : (bin : Binary v) -> Binary (v * 2)

  ||| A 1-bit
  ||| Build a binary number equal to 2v + 1
  ||| @bin the rest of this binary number
  ||| @v the value of the rest of this binary number
  B1    : (bin : Binary v) -> Binary (S (v * 2))

implementation Show (Binary k) where
  show BEnd = ""
  show (B0 bin) = show bin ++ "0"
  show (B1 bin) = show bin ++ "1"

-- we would like to derive a `Num` instance for `Binary` but unfortunately
-- we can't because `Binary` has a type-level argument hence it's not possible
-- to provide meaning to `Num` operations
--
-- We therefore resort to work with specialized functions for casting and
-- operating on binary numbers.
fromNat : (n : Nat) -> Binary n
fromNat Z   = BEnd
fromNat (S k) with (evenOrOdd k)
  | (Left (half ** prf)) = rewrite prf in B1 (fromNat half)
  | (Right (haf ** prf)) = rewrite prf in B0 (fromNat (S haf))


infixl 6 +.

-- there's probably a way to use lemmas from Prelude.Nat about distributivity
-- of multiplication....
distributePlus2 : (k : Nat) -> (S (S (k * 2)) = (k + 1) * 2)
distributePlus2 Z     = Refl
distributePlus2 (S k) =
  let hyp = distributePlus2 k
  in rewrite hyp in Refl

||| Add two binary numbers
||| Unsurprisingly, it returns the sum of the 2 numbers, in binary representation.
||| @x left number to add
||| @y right number to add
(+.) : (x : Binary n) -> (y : Binary m) -> Binary (n + m)
(+.) BEnd     y             = y
(+.) x        BEnd  {n}     = rewrite plusZeroRightNeutral n in x
(+.) (B0 l)  (B0 r) {n=v * fromInteger 2}     {m=w * fromInteger 2}     =
  rewrite sym (multDistributesOverPlusLeft v w 2) in B0 (l +. r)
(+.) (B1 l)  (B0 r) {n=S (v * fromInteger 2)} {m=w * fromInteger 2}     =
  rewrite sym (multDistributesOverPlusLeft v w 2) in B1 (l +. r)
(+.) (B0 l)  (B1 r) {n=v * fromInteger 2}     {m=S (w * fromInteger 2)} =
  rewrite plusCommutative (mult v 2) (S (mult w 2)) in
  rewrite sym (multDistributesOverPlusLeft w v 2) in B1 (r +. l)
(+.) (B1 l)  (B1 r) {n=S (v * fromInteger 2)} {m=S (w * fromInteger 2)} =
  rewrite plusCommutative (mult v 2) (S (mult w 2)) in
  rewrite sym (multDistributesOverPlusLeft w v 2) in
  rewrite distributePlus2 (w+v) in B0 (r +. l +. B1 BEnd)
