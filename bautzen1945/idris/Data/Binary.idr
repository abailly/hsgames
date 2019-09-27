||| Binary representation of natural numbers
||| See:
||| * [Idris tutorial](https://github.com/idris-lang/idris-tutorial/blob/master/examples/binary.idr)
||| * [Haskell](https://jaspervdj.be/posts/2018-09-04-binomial-heaps-101.html#binomial-forests) Binomial heap
|||   making use of binary numbers
module Data.Binary

import public Data.Nat.Parity

%access public export
%default total

||| Right to left binary representation of numbers.
||| Working with a right-to-left representation makes addition much easier. We
||| can always reconstruct the usual representation through a `Show` instance
||| for example.
|||
||| ```
||| > :x toBinary 12
||| B0 (B0 (B1 (B1 BZero))) : Binary 12
||| ```
data Binary : (value : Nat) -> Type where
  ||| Denote the single 0 number in binary representation
  ||| This is also used to _close_ a sequence of binary digits
  BZero  : Binary Z

  ||| A `0` bit
  ||| Build a binary number equal to 2v. Note that because we don't want to have
  ||| multiple representations for the number 0, eg. `BZero` or an arbitrary
  ||| string of `B0` terminated by `BZero`, we require the next `bin`ary to be
  ||| strictly positive.
  ||| @bin the rest of this binary number
  ||| @n the value of the rest of this binary number.
  B0    : (bin : Binary (S n)) -> Binary ((S n) * 2)

  ||| A `1` bit
  ||| Build a binary number equal to 2v + 1
  ||| @bin the rest of this binary number
  ||| @n the value of the rest of this binary number
  B1    : (bin : Binary n) -> Binary (S (n * 2))

implementation Show (Binary k) where
  show BZero      = "0"
  show (B0 bin)   = show bin ++ "0"
  show (B1 BZero) = "1"
  show (B1 bin)   = show bin ++ "1"

show_test_0 : show BZero = "0"
show_test_0 = Refl

show_test_10 : show (B0 (B1 BZero)) = "10"
show_test_10 = Refl

show_test_1 : show (B1 BZero) = "1"
show_test_1 = Refl

-- there's probably a way to use lemmas from Prelude.Nat about distributivity
-- of multiplication....
distributePlus2 : (k : Nat) -> (S (S (k * 2)) = (k + 1) * 2)
distributePlus2 Z     = Refl
distributePlus2 (S k) =
  let hyp = distributePlus2 k
  in rewrite hyp in Refl


||| Increment given `Binary`
inc : Binary k -> Binary (S k)
inc BZero = B1 BZero
inc (B0 bin) = B1 bin
inc (B1 bin) = B0 (inc bin)

-- we would like to derive a `Num` instance for `Binary` but unfortunately
-- we can't because `Binary` has a type-level argument hence it's not possible
-- to provide meaning to `Num` operations
--
-- We therefore resort to work with specialized functions for casting and
-- operating on binary numbers.
toBinary : (n : Nat) -> Binary n
toBinary Z     = BZero
toBinary (S k) = inc (toBinary k)

lemma1 : (n : Nat) -> toBinary (S n * 2) = B0 (toBinary (S n))
lemma1 Z     = Refl
lemma1 (S k) = rewrite lemma1 k in Refl

lemma2 : (n : Nat) -> (B1 (toBinary n) = inc (toBinary (mult n 2)))
lemma2 Z     = Refl
lemma2 (S k) = rewrite sym (lemma2 k) in Refl

||| A proof that we can convert a `Binary` from a `Nat` and get the
||| same "number".
||| @b a `Binary` number
||| @n a `Nat` representing the same number than b
natIsBinary : (b : Binary n) -> (b = toBinary n)
natIsBinary BZero = Refl
natIsBinary (B0 bin {n}) = rewrite natIsBinary bin in
                           rewrite lemma1 n in Refl
natIsBinary (B1 bin {n}) = rewrite natIsBinary bin in lemma2 n

Zero : Binary 0
Zero = BZero

One : Binary 1
One = toBinary 1

Two : Binary 2
Two = toBinary 2

infixl 6 +.

lemmaB0plusB0 : (m, n : Nat) -> (S (plus n (S m)) * 2) =
       (S (S (plus (mult n 2) (S (S (mult m 2))))))
lemmaB0plusB0 m n =
  rewrite plusCommutative n (S m) in
  rewrite plusCommutative (mult n 2) (S (S (mult m 2))) in
  rewrite multDistributesOverPlusLeft m n 2 in
  Refl

lemmaB1plusB0 : (v, w : Nat) -> (mult (v + S w) 2 = (mult v 2) + (S (S (mult w 2))))
lemmaB1plusB0 v w =
  rewrite multDistributesOverPlusLeft v (S w) 2 in
  Refl

lemmaB0plusB1 : (v, w : Nat) -> (mult (S v + w) 2 = S (plus (mult v 2) (S (mult w 2))))
lemmaB0plusB1 v w =
  rewrite multDistributesOverPlusLeft v w 2 in
  rewrite plusSuccRightSucc (mult v 2) (mult w 2) in
  Refl

lemmaB1plusB1 : (v, w : Nat) -> (S (mult (v + w) 2) = plus (mult v 2) (S (mult w 2)))
lemmaB1plusB1 v w =
  rewrite multDistributesOverPlusLeft v w 2 in
  rewrite plusSuccRightSucc (mult v 2) (mult w 2) in
  Refl

||| Add two binary numbers
|||
||| Unsurprisingly, it returns the sum of the 2 numbers, in binary representation.
||| **Implementation note**: We use `toBinary` to convert the sum of the 2 numbers
||| which is probably very inefficient at runtime, probably in `O(n + m)` whereas
||| standard addition has complexity `O(log(n + m))`. This is left to implement
||| properly later on, using the first implementation provided in the comments
||| below.
|||
||| @x left binary number to add
||| @y right binary number to add
||| @n `Nat` represented by `x`
||| @m `Nat` represented by `y`
add : (x : Binary n) -> (y : Binary m) -> Binary (n + m)
add BZero         y            = y
add (B0 bin {n})  BZero        = rewrite plusZeroRightNeutral (mult n 2) in B0 bin
add (B1 bin {n})  BZero        = rewrite plusZeroRightNeutral (mult n 2) in B1 bin
add (B0 l {n=v})  (B0 r {n=w}) = rewrite sym (lemmaB0plusB0 w v) in B0 (add l r)
add (B1 l {n=v})  (B0 r {n=w}) = rewrite sym (lemmaB1plusB0 v w) in B1 (add l r)
add (B0 l {n=v})  (B1 r {n=w}) = rewrite sym (lemmaB0plusB1 v w) in B1 (add l r)
add (B1 l {n=v})  (B1 r {n=w}) = rewrite sym (lemmaB1plusB1 v w) in B0 (inc (add l r))

(+.) : (x : Binary n) -> (y : Binary m) -> Binary (n + m)
(+.) = add


-- Properties
binaryInversion : (n : Nat) -> (bin : Binary n) ->
                  Either (n = 0, bin = BZero)  $
                  Either (k : Nat **
                          (bin' : Binary (S k) ** (n = mult (S k) 2, bin = B0 bin' )))
                         (k : Nat **
                          (bin' : Binary k ** (n = S (mult k 2), bin = B1 bin' )))
binaryInversion Z                       BZero      = Left (Refl, Refl)
binaryInversion (S _)                   BZero      impossible
binaryInversion ((S k) * fromInteger 2) (B0 bin)   = Right (Left (k ** (bin ** (Refl, Refl))))
binaryInversion (S (Z * fromInteger 2)) (B1 BZero) = Right (Right (0 ** (BZero ** (Refl, Refl))))
binaryInversion (S (k * fromInteger 2)) (B1 bin)   = Right (Right (k ** (bin ** (Refl, Refl))))

injectivity : (n1, n2 : Nat) -> (n1 = n2) -> (b1 : Binary n1) -> (b2 : Binary n2) -> b1 = b2
injectivity n n Refl b1 b2 = trans (natIsBinary b1) (sym $ natIsBinary b2)

bZeroRightNeutral : {n : Nat} -> (b : Binary n) -> (add b BZero = b)
bZeroRightNeutral {n} b = injectivity (n + 0) n (plusZeroRightNeutral n) (add b BZero) b
