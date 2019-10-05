module Prelude.Enum

import Data.List
import Data.Nat

export
Cast Int Nat where
  cast i = fromInteger (cast i)

export
Cast Integer Nat where
  cast = fromInteger

export
natRange : Nat -> List Nat
natRange n = List.reverse (go n)
  where go : Nat -> List Nat
        go Z = []
        go (S n) = n :: go n

-- predefine Nat versions of Enum, so we can use them in the default impls
export
countFrom : Num n => n -> n -> Stream n
countFrom start diff = start :: countFrom (start + diff) diff

export total
natEnumFromThen : Nat -> Nat -> Stream Nat
natEnumFromThen n next = countFrom n (minus next n)

export total
natEnumFromTo : Nat -> Nat -> List Nat
natEnumFromTo n m = if n <= m
                    then go n m
                    else List.reverse $ go m n
  where go : Nat -> Nat -> List Nat
        go n m = map (plus n) (natRange (minus (S m) n))

export total
natEnumFromThenTo' : Nat -> Nat -> Nat -> List Nat
natEnumFromThenTo' _ Z       _ = []
natEnumFromThenTo' n (S inc) m = map (plus n . (* (S inc))) (natRange (S (divNatNZ (minus m n) (S inc) SIsNotZ)))

export total
natEnumFromThenTo : Nat -> Nat -> Nat -> List Nat
natEnumFromThenTo n next m = if n == m then [n]
                             else if n < m then natEnumFromThenTo' n (minus next n) m
                             else case minus n next of
                                  Z => []
                                  S step => List.reverse . map (+ (modNatNZ (minus n m) (S step) SIsNotZ)) $ natEnumFromThenTo' m (minus n next) n
    where modNatNZ : Nat -> (m : Nat) -> Not (m = 0) -> Nat
          modNatNZ n m nz = minus n $ mult m $ divNatNZ n m nz

public export
interface Enum a where
  total pred : a -> a
  total succ : a -> a
  succ e = fromNat (S (toNat e))
  total toNat : a -> Nat
  total fromNat : Nat -> a
  total enumFrom : a -> Stream a
  enumFrom n = n :: enumFrom (succ n)
  total enumFromThen : a -> a -> Stream a
  enumFromThen x y = map fromNat (natEnumFromThen (toNat x) (toNat y))
  total enumFromTo : a -> a -> List a
  enumFromTo x y = map fromNat (natEnumFromTo (toNat x) (toNat y))
  total enumFromThenTo : a -> a -> a -> List a
  enumFromThenTo x1 x2 y = map fromNat (natEnumFromThenTo (toNat x1) (toNat x2) (toNat y))

public export
Enum Nat where
  pred n = Nat.pred n
  succ n = S n
  toNat x = id x
  fromNat x = id x
  enumFromThen x y = natEnumFromThen x y
  enumFromThenTo x y z = natEnumFromThenTo x y z
  enumFromTo x y = natEnumFromTo x y

public export
Enum Integer where
  pred n = n - 1
  succ n = n + 1
  toNat n = cast n
  fromNat n = cast n
  enumFromThen n inc = countFrom n (inc - n)
  enumFromTo n m = if n <= m
                   then go n m
                   else List.reverse $ go m n
    where go' : Integer -> List Nat -> List Integer
          go' _ [] = []
          go' n (x :: xs) = n + cast x :: go' n xs
          go : Integer -> Integer -> List Integer
          go n m = go' n (natRange (S (cast {to = Nat} (m - n))))
  enumFromThenTo n next m = if n == m then [n]
                            else if next - n == 0 || (next - n < 0) /= (m - n < 0) then []
                            else go (natRange (S (divNatNZ (fromInteger (abs (m - n))) (S (fromInteger ((abs (next - n)) - 1))) SIsNotZ)))
    where go : List Nat -> List Integer
          go [] = []
          go (x :: xs) = n + (cast x * (next - n)) :: go xs

public export
Enum Int where
  pred n = n - 1
  succ n = n + 1
  toNat n = cast n
  fromNat n = cast n
  enumFromTo n m = if n <= m
                   then go n m
                   else List.reverse $ go m n
    where go' : List Int -> Nat -> Int -> List Int
          go' acc Z     m = m :: acc
          go' acc (S k) m = go' (m :: acc) k (m - 1)
          go : Int -> Int -> List Int
          go n m = go' [] (cast {to = Nat} (m - n)) m
  enumFromThen n inc = countFrom n (inc - n)

  enumFromThenTo n next m = if n == m then [n]
                            else if next - n == 0 || (next - n < 0) /= (m - n < 0) then []
                            else go (natRange (S (divNatNZ (cast {to=Nat} (abs (m - n))) (S (cast {to=Nat} ((abs (next - n)) - 1))) SIsNotZ)))
    where go : List Nat -> List Int
          go [] = []
          go (x :: xs) = n + (cast x * (next - n)) :: go xs

public export
Enum Char where
  toNat c   = toNat (fromInteger $ cast c)
  fromNat n = cast (cast n {to=Int})

  pred c = fromNat (pred (toNat c))

-- syntax "[" [start] ".." [end] "]"
--      = enumFromTo start end
-- syntax "[" [start] "," [next] ".." [end] "]"
--      = enumFromThenTo start next end

-- syntax "[" [start] ".." "]"
--      = enumFrom start
-- syntax "[" [start] "," [next] ".." "]"
--      = enumFromThen start next
