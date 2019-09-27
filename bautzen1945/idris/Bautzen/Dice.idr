module Bautzen.Dice

import Data.Fin

%access export

||| A random `n`-sided Dice.
|||
||| A `Dice` encapsulates a `roll` result and a random `seed` that's used
||| to generate another `Dice` instance.
data Dice : (sides : Nat) -> Type where
  MkDice : (result : Fin sides) -> (seed : Integer) -> Dice sides

implicit roll : Dice n -> Fin n
roll (MkDice result seed) = result
