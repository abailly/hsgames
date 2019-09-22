||| Combat logic for _Bautzen1945_
||| Section 8
module Bautzen.Game.Combat

import Bautzen.GameUnit
import Bautzen.Game.Core
import Bautzen.Odds
import Bautzen.Pos
import Bautzen.Terrain

import Data.Nat.DivMod
import Data.Vect as V

%access export
%default total

-- section 8.1
-- Those are generalities about the combat that cannot be implemented alone

-- section 8.2

||| Start resolving an attack from given list of `attackers` to given list of `defenders`.
|||
||| This computes the base factors for the combat, without taking into account the support
||| provided by HQs, Artillery, etc. nor the change to odds coming from terrain...
||| * see section 8.2, alinea 3
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

||| Combat result as steps lost by attacker and defender.
record Losses where
  constructor (/>)

  ||| Steps lost by attacker
  attackerLoss : Nat

  ||| Steps lost by defender. Can be transformed in hexes of retreat.
  defenderLoss : Nat

infix 1 />


||| Combat resolution table.
|||
||| The table is transposed w.r.t. the actual rules booklet
CombatTable : Vect 8 (Vect 8 Losses)
CombatTable =
  [ [ 3 /> 0, 3 /> 0, 3 /> 0, 2 /> 0 , 2 /> 1 , 2 /> 1 , 1 /> 1 , 0 /> 1 ]
  , [ 2 /> 0, 2 /> 0, 1 /> 0, 1 /> 0 , 2 /> 1 , 2 /> 2 , 2 /> 2 , 1 /> 2 ]
  , [ 1 /> 0, 1 /> 0, 1 /> 0, 1 /> 1 , 1 /> 1 , 1 /> 2 , 0 /> 2 , 0 /> 2 ]
  , [ 1 /> 0, 1 /> 0, 1 /> 1, 1 /> 2 , 1 /> 2 , 1 /> 2 , 1 /> 2 , 1 /> 3 ]
  , [ 1 /> 1, 1 /> 1, 1 /> 2, 1 /> 2 , 1 /> 2 , 1 /> 3 , 1 /> 3 , 1 /> 4 ]
  , [ 1 /> 1, 0 /> 1, 1 /> 3, 1 /> 3 , 1 /> 4 , 0 /> 4 , 0 /> 4 , 0 /> 5 ]
  , [ 0 /> 1, 0 /> 1, 1 /> 3, 1 /> 3 , 1 /> 4 , 0 /> 4 , 0 /> 4 , 0 /> 5 ]
  , [ 0 /> 2, 0 /> 2, 1 /> 3, 1 /> 3 , 1 /> 4 , 0 /> 5 , 0 /> 5 , 0 /> 6 ]
  ]


||| Given some odds and the result of 6-sided dice, provide combat outcome in the form of
||| a pair of numbers representing step loss for each side.
|||
||| * section 8.2, alinea 6
|||
||| @odds the final odds of the combat
||| @modifiedDiceRoll the result of the dice, with modifiers applied
resolve : (odds : Odds) -> (modifiedDiceRoll : Fin 8) -> Losses
resolve odds dice = dice `index` (odds `index` CombatTable)

findUnits : List String -> List (GameUnit, Pos) -> Either GameError (List (GameUnit, Pos))
findUnits names positions =
  case partitionEithers units of
       ([], us) => Right us
       (errs, _) => Left $ NoSuchUnits errs
  where
    findUnit : String -> Maybe (GameUnit, Pos)
    findUnit n = find (\ (u, p) => fullName u == n) positions

    units : List (Either String (GameUnit, Pos))
    units = map (\ n => case findUnit n of
                             Nothing => Left n
                             Just up => Right up) names

validateAttackers : (side : Side) -> (units : List (GameUnit, Pos)) -> (gameMap : Map)
                  -> (attackers : List (GameUnit, Pos)) -> (target : Pos)
                  -> Either GameError (List (GameUnit, Pos))
validateAttackers curSide units gameMap attackers target =
  if not (all (\ (u, _) => side (nation u) == curSide) attackers)
  then Left (NotYourTurn curSide)
  else Right attackers

checkAttackersAreAdjacentToTarget : (attackers : List (GameUnit, Pos)) -> (target : Pos)
                                  -> Either GameError (List (GameUnit, Pos))
checkAttackersAreAdjacentToTarget attackers target =
  case filter (\ (_, p) => distance p target /= 1) attackers of
    [] => Right attackers
    errs => Left (NotAdjacentTo (map fst errs) target)


||| Start a combat with given units attacking given hex.
attackWith : (side : Side) -> (units : List (GameUnit, Pos)) -> (gameMap : Map)
           -> (unitNames : List String) -> (target : Pos)
           -> Either GameError Event
attackWith side units gameMap unitNames target = do
  attackUnits <- findUnits unitNames units
  attackers <- validateAttackers side units gameMap attackUnits target
  -- section 8.1
  attackers' <- checkAttackersAreAdjacentToTarget attackers target
  ?noEvent

namespace CombatTest
  %access private

  positions : List (GameUnit, Pos)
  positions = [ (GameUnit.p13_5dp, Hex 5 4)
              , (GameUnit.g21_20pz, Hex 4 4)
              , (GameUnit.g59_20pz, Hex 3 4)
              ]

  basic_odds_are_Sum_of_attack_over_sum_of_defense : attack [ GameUnit.g21_20pz ] [ Bautzen.GameUnit.p13_5dp ] = (MkRawOdds 6 4)
  basic_odds_are_Sum_of_attack_over_sum_of_defense = Refl

  adds_support_Factors_to_raw_odds : support [ GameUnit.p6l ] [ GameUnit.g20pz ] (MkRawOdds 6 7) =  (MkRawOdds 10 13)
  adds_support_Factors_to_raw_odds = Refl

  support_factors_can_excede_to_raw_odds : support [ GameUnit.p6l ] [ GameUnit.g20pz ] (MkRawOdds 6 4) =  (MkRawOdds 10 8)
  support_factors_can_excede_to_raw_odds = Refl

  fail_attack_if_names_do_not_exist : attackWith Axis CombatTest.positions TestMap [ "21/20Pz", "foo" ] (Hex 5 4) = Left (NoSuchUnits [ "foo" ])
  fail_attack_if_names_do_not_exist = Refl

  fail_attack_if_attacking_with_wrong_side : attackWith Axis CombatTest.positions TestMap [ "13/5DP" ] (Hex 5 4) = Left (NotYourTurn Axis)
  fail_attack_if_attacking_with_wrong_side = Refl

  fail_attack_if_not_adjacent_to_target_hex : attackWith Axis CombatTest.positions TestMap [ "59/20Pz", "21/20Pz" ] (Hex 5 4) = Left (NotAdjacentTo [ GameUnit.g59_20pz ] (Hex 5 4))
  fail_attack_if_not_adjacent_to_target_hex = Refl
