module Data.Nat.Extra

import Data.Nat

public export
(-) : (m : Nat) -> (n : Nat) -> {auto smaller : LTE n m} -> Nat
(-) m n {smaller} = minus m n
