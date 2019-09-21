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

||| Start resolving an attack from given list of `attackers` to given `defender` Position.
|||
||| This computes the base factors for the combat, without taking into account the support
||| provided by HQs, Artillery, etc. nor the change to odds coming from terrain...
|||
||| @attackers the group of units attacking
||| @defender the position that's under attack
||| @units the current state and position of units
||| @gameMap the terrain map
attack : (attackers : List GameUnit) -> (defender : Pos) -> (units : List (GameUnit, Pos)) -> (gameMap : Map) -> Either GameError RawOdds
attack attackers defender units gameMap = Right $ MkRawOdds atk def
  where
    atk = sum $ map attackCapacity attackers
    def = sum $ map defenseCapacity $ map fst $ filter ( \ (u,p) => p == defender) units


namespace CombatTest
  %access private

  positions : List (GameUnit, Pos)
  positions = [ (Bautzen.GameUnit.p13_5dp, Hex 5 4)
              , (Bautzen.GameUnit.g21_20pz, Hex 4 4)
              ]

  basic_odds_are_Sum_of_attack_over_sum_of_defense : attack [ GameUnit.g21_20pz ] (Hex 5 4) CombatTest.positions TestMap = Right (MkRawOdds 6 4)
  basic_odds_are_Sum_of_attack_over_sum_of_defense = Refl
