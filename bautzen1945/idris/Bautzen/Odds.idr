||| Logic for manipulating combat odds
module Bautzen.Odds

import Data.Fin
import Data.Nat.DivMod

%access public export
%default total

||| The raw odds in a combat, basically a glorified pair.
record RawOdds where
  constructor MkRawOdds

  ||| Raw attack factor
  attackFactor : Nat

  ||| Raw defense factor
  ||| TODO make it a positive so that we prevent stupid odds from
  ||| happening
  defenseFactor : Nat

Semigroup RawOdds where
  (<+>) (MkRawOdds atk def) (MkRawOdds atk' def') = MkRawOdds (atk + atk') (def + def')

||| The actual odds representing a column in the combat table
data Odds : Type where
  OneVsTwo : Odds
  OneVsOne : Odds
  TwoVsOne : Odds
  ThreeVsOne : Odds
  FourVsOne : Odds
  FiveVsOne : Odds
  SixVsOne : Odds
  SevenVsOne : Odds

Eq Odds where
  OneVsTwo == OneVsTwo = True
  OneVsOne == OneVsOne = True
  TwoVsOne == TwoVsOne = True
  ThreeVsOne == ThreeVsOne = True
  FourVsOne == FourVsOne = True
  FiveVsOne == FiveVsOne = True
  SixVsOne == SixVsOne = True
  SevenVsOne == SevenVsOne = True
  _ == _ = False

Enum Odds where
  pred OneVsTwo = OneVsTwo
  pred OneVsOne = OneVsTwo
  pred TwoVsOne = OneVsOne
  pred ThreeVsOne = TwoVsOne
  pred FourVsOne = ThreeVsOne
  pred FiveVsOne = FourVsOne
  pred SixVsOne = FiveVsOne
  pred SevenVsOne = SixVsOne

  succ OneVsTwo = OneVsOne
  succ OneVsOne = TwoVsOne
  succ TwoVsOne = ThreeVsOne
  succ ThreeVsOne = FourVsOne
  succ FourVsOne = FiveVsOne
  succ FiveVsOne = SixVsOne
  succ SixVsOne = SevenVsOne
  succ SevenVsOne = SevenVsOne

  toNat OneVsTwo = 0
  toNat OneVsOne = 1
  toNat TwoVsOne = 2
  toNat ThreeVsOne = 3
  toNat FourVsOne = 4
  toNat FiveVsOne = 5
  toNat SixVsOne = 6
  toNat SevenVsOne = 7

  fromNat Z = OneVsTwo
  fromNat (S k) = succ (fromNat k)

Ord Odds where
  compare x y = compare (toNat x) (toNat y)

MinBound Odds where
  minBound = OneVsTwo

MaxBound Odds where
  maxBound = SevenVsOne

implicit toFin : Odds -> Fin 8
toFin OneVsTwo = 0
toFin OneVsOne = 1
toFin TwoVsOne = 2
toFin ThreeVsOne = 3
toFin FourVsOne = 4
toFin FiveVsOne = 5
toFin SixVsOne = 6
toFin SevenVsOne = 7

||| Transform a concrete attack/defense ratio into `Odds`
|||
||| The quotient is rounded to the lowest integer to produce `Odds`. By convention
||| if the `defenseFactor` is 0 the ratio is the greatest possible. This situation
||| should not happen
|||
||| @odds raw odds to transform into a column
odds : (odds: RawOdds) -> Odds
odds (MkRawOdds attackFactor Z) = maxBound -- by convention, should not happen
odds (MkRawOdds attackFactor (S def)) with (attackFactor `divMod` def)
  odds (MkRawOdds (r + (q * (S def))) (S def)) | (MkDivMod q r _) = fromNat q

infixl 6 >>>
infixl 6 <<<

||| Shift odds by a given number of steps "up".
|||
||| @odds original odds
||| @steps number of steps to shift odds
(>>>) : (odds : Odds) -> (steps : Nat) -> Odds
c >>> Z = c
c >>> (S k) = foldl (const . succ) c [0.. k]

||| Shift odds by a given number of steps "down".
|||
||| @odds original odds
||| @steps number of steps to shift odds
(<<<) : (odds : Odds) -> (steps : Nat) -> Odds
c <<< Z = c
c <<< (S k) = foldl (const . pred) c [0.. k]

namespace OddsTest
  %access private

  odds_are_rounded_down : odds (MkRawOdds 3 2) = OneVsOne
  odds_are_rounded_down = Refl

  odds_are_capped : odds (MkRawOdds 20 2) = SevenVsOne
  odds_are_capped = Refl

  odds_are_floored : odds (MkRawOdds 2 20) = OneVsTwo
  odds_are_floored = Refl
