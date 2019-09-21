||| Combat logic for _Bautzen1945_
||| Section 8
module Bautzen.Game.Combat

import Bautzen.GameUnit
import Bautzen.Game.Core
import Bautzen.Pos
import Bautzen.Terrain

-- section 8.1
-- Those are generalities about the combat that cannot be implemented alone

-- section 8.2

||| The raw odds in a combat, basically a glorified pair.
record RawOdds where
  constructor MkRawOdds

  ||| Raw attack factor
  attackFactor : Nat

  ||| Raw defense factor
  defenseFactor : Nat

Semigroup RawOdds where
  (<+>) (MkRawOdds atk def) (MkRawOdds atk' def') = MkRawOdds (atk + atk') (def + def')

||| Start resolving an attack from given list of `attackers` to given list of `defenders`.
|||
||| see section 8.2, alinea 3
||| This computes the base factors for the combat, without taking into account the support
||| provided by HQs, Artillery, etc. nor the change to odds coming from terrain...
|||
||| @attackers the group of units attacking
||| @defenders the group of defending units
attack : (attackers : List GameUnit) -> (defenders : List GameUnit) -> RawOdds
attack attackers defenders = MkRawOdds atk def
  where
    atk = sum $ map attackCapacity attackers
    def = sum $ map defenseCapacity defenders

||| Update some `odds` with all relevant support factors.
|||
||| * see section 8.2, alinea 3
||| * see section 9.2
|||
||| @attackSupport list of units supporting the attack
||| @defenseSupport list of units supporting the defense
||| @baseOdds the base odds without support
support : (attackSupport : List GameUnit) -> (defenseSupport : List GameUnit) -> (baseOdds : RawOdds) -> RawOdds
support attackSupport defenseSupport baseOdds@(MkRawOdds atk def) = baseOdds <+> MkRawOdds atkSupport defSupport
  where
    atkSupport = min atk $ sum (map supportCapacity attackSupport)
    defSupport = min def $ sum (map supportCapacity defenseSupport)


namespace CombatTest
  %access private

  positions : List (GameUnit, Pos)
  positions = [ (Bautzen.GameUnit.p13_5dp, Hex 5 4)
              , (Bautzen.GameUnit.g21_20pz, Hex 4 4)
              ]

  basic_odds_are_Sum_of_attack_over_sum_of_defense : attack [ GameUnit.g21_20pz ] [ Bautzen.GameUnit.p13_5dp ] = (MkRawOdds 6 4)
  basic_odds_are_Sum_of_attack_over_sum_of_defense = Refl

  adds_support_Factors_to_raw_odds : support [ GameUnit.p6l ] [ GameUnit.g20pz ] (MkRawOdds 6 7) =  (MkRawOdds 10 13)
  adds_support_Factors_to_raw_odds = Refl

  support_factors_can_excede_to_raw_odds : support [ GameUnit.p6l ] [ GameUnit.g20pz ] (MkRawOdds 6 4) =  (MkRawOdds 10 8)
  support_factors_can_excede_to_raw_odds = Refl
