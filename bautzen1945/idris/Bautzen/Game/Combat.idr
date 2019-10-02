||| Combat logic for _Bautzen1945_
||| Section 8
module Bautzen.Game.Combat

import Bautzen.Combats
import Bautzen.GameUnit
import Bautzen.Game.Command
import Bautzen.Game.Core
import Bautzen.Odds
import Bautzen.Pos
import Bautzen.Terrain

import Data.Nat.DivMod
import Data.Vect as V





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

||| Given some odds and the result of 6-sided dice, provide combat outcome in the form of
||| a pair of numbers representing step loss for each side.
|||
||| * section 8.2, alinea 6
|||
||| @odds the final odds of the combat
||| @modifiedDiceRoll the result of the dice, with modifiers applied
resolve : (odds : Odds) -> (modifiedDiceRoll : Fin 8) -> Losses
resolve odds dice = dice `index` (odds `index` CombatTable)

findUnit : String -> { positions : List (GameUnit, Pos) } -> Maybe (GameUnit, Pos)
findUnit n {positions} = find (\ (u, p) => fullName u == n) positions

findUnits : List String -> (positions : List (GameUnit, Pos)) -> Either GameError (List (GameUnit, Pos))
findUnits names positions =
  case partitionEithers units of
       ([], us) => Right us
       (errs, _) => Left $ NoSuchUnits errs
  where
    units : List (Either String (GameUnit, Pos))
    units = map (\ n => case findUnit n {positions} of
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

validateDefenders : (side : Side) -> (positions : List (GameUnit, Pos)) -> (gameMap : Map)
                  -> (target : Pos)
                  -> Either GameError (List (GameUnit, Pos))
validateDefenders attackerSide positions gameMap target =
  case filter (\ (u, p) => p == target) positions of
    [] => Left $ NothingToAttack target
    defenders => if any (\ (u, _) => side (nation u) == attackerSide) defenders
                 then Left $ AttackingOwnUnits (map fst defenders) target
                 else Right defenders

||| Start a combat with given units attacking given hex.
attackWith : (side : Side) -> (units : List (GameUnit, Pos)) -> (gameMap : Map)
           -> (unitNames : List String) -> (target : Pos)
           -> Either GameError Event
attackWith side units gameMap unitNames target = do
  attackUnits <- findUnits unitNames units
  attackers <- validateAttackers side units gameMap attackUnits target
  -- section 8.1
  attackers' <- checkAttackersAreAdjacentToTarget attackers target
  defenders <- validateDefenders side units gameMap target
  pure $ CombatEngaged attackers defenders target

-- TODO units cannot be part of an attack twice

-- Support

checkSupportingUnits : (units : List (GameUnit, Pos))
                     -> Either GameError (List (GameUnit, Pos))
checkSupportingUnits units =
  let notSupporting = filter (not . isSupportingUnit . fst) units
  in case notSupporting of
       [] => Right units
       ns => Left $ NotSupportingUnits (map fst ns)

supportInRangeOf : (base : List (GameUnit, Pos)) -> (supporter: (GameUnit, Pos)) -> Bool
supportInRangeOf base (unit, pos) =
  let maxDistance = supportDistance unit
  in any (\ (u, p) => distance p pos <= maxDistance) base

checkSupportInRange : (base : List (GameUnit, Pos)) -> (supporters : List (GameUnit, Pos)) -> Either GameError (List (GameUnit, Pos))
checkSupportInRange base supporters =
  case filter (not . supportInRangeOf base) supporters of
    [] => Right supporters
    xs => Left $ NotInSupportRange (map fst xs)

checkChainOfCommand : (base : List (GameUnit, Pos)) -> (supporters : List (GameUnit, Pos)) -> Either GameError (List (GameUnit, Pos))
checkChainOfCommand base supporters =
  case filter (\ (u, _) => not (any (isHQFor u) $ map fst base)) hqs of
    [] => Right supporters
    xs => Left $ NotInChainOfCommand (map fst xs)
  where
    hqs : List (GameUnit, Pos)
    hqs = filter (isHQ . fst) supporters

validateSupport :  (supported : EngagedUnits)
                -> (supporters : List (GameUnit, Pos))
                -> Either GameError (List (GameUnit, Pos))
validateSupport (MkEngagedUnits base _ _) supporters =
  checkSupportingUnits supporters >>=
  checkSupportInRange base >>=
  checkChainOfCommand base

validateSupportUnits : (currentSide : Side) -> (supportedSide : Side)
                     -> (state : CombatState)
                     -> (units : List (GameUnit, Pos))
                     -> Either GameError (List (GameUnit, Pos))
validateSupportUnits currentSide supportedSide (MkCombatState _ atk def _) units =
  if (currentSide == supportedSide)
  then validateSupport atk units
  else validateSupport def units


||| Add support units to an _engaged_ combat.
|||
||| If successful, this will add some units to
|||
||| @currentSide the side whose turn it is to play, e.g the attacker's side
||| @supportSide the side which is expected to provide support
||| @units the positions and state of units on the map
||| @gameMap the map
||| @unitNames names of units providing support
||| @state overall state of the combat
supportWith : (currentSide : Side) -> (supportSide : Side)
           -> (units : List (GameUnit, Pos)) -> (gameMap : Map)
           -> (unitNames : List String)
           -> (state : CombatState)
           -> Either GameError Event
supportWith currentSide supportSide units gameMap unitNames state = do
  supportUnits <- findUnits unitNames units >>= validateSupportUnits currentSide supportSide state
  pure $ TacticalSupportProvided supportSide supportUnits

-- section 7.2
findSupportColumn : (supportSide : Side) -> (hex : Pos) -> (units : List (GameUnit, Pos)) -> Either GameError (GameUnit, Pos)
findSupportColumn supportSide hex units =
  case find (\ (u, p) => p == hex &&  unitType u == SupplyColumn) units of
    Nothing => Left (NoSupplyColumnThere hex)
    Just sc => Right sc

--- section 7.2.1

checkSCNeighboursEngagedUnits :
  List (GameUnit, Pos) -> (GameUnit, Pos) -> Either GameError (GameUnit, Pos)
checkSCNeighboursEngagedUnits base sc@(unit, hex) =
  if any (\ (u, p) => p == hex || (hex  `elem` neighbours p)) base
  then Right sc
  else Left (NotInSupportRange [unit] )

--- section 7.2.2

checkSomeUnitsAreInCommand :
  List (GameUnit, Pos) -> List (GameUnit, Pos) -> (GameUnit, Pos) -> Either GameError (GameUnit, Pos)
checkSomeUnitsAreInCommand base units sc =
  if any (underCommand units) (map fst base)
  then Right sc
  else Left (NotInChainOfCommand [fst sc])

validateSC : EngagedUnits -> List (GameUnit, Pos) -> (GameUnit, Pos) -> Either GameError (GameUnit, Pos)
validateSC (MkEngagedUnits base _ _) units sc =
  checkSCNeighboursEngagedUnits base sc >>=
  checkSomeUnitsAreInCommand base units

validateSupportFromSC : (currentSide : Side) -> (supportSide : Side)
                      -> (state : CombatState)
                      -> (units : List (GameUnit, Pos))
                      -> (unit : (GameUnit, Pos))
                      -> Either GameError (GameUnit, Pos)
validateSupportFromSC currentSide supportSide (MkCombatState _ atk def _) units unit =
  if currentSide == supportSide
  then validateSC atk units unit
  else validateSC def units unit

||| Use a `SupplyColumn` type of unit as "Strategic" support fort the combat
|||
||| @currentSide the side whose turn it is to play, e.g the attacker's side
||| @supportSide the side which is expected to provide support
||| @units the positions and state of units on the map
||| @gameMap the map
||| @scLocation location of the supply column
||| @state overall state of the combat
useSupplyColumn : (currentSide : Side) -> (supportSide : Side)
           -> (units : List (GameUnit, Pos)) -> (gameMap : Map)
           -> (scLocation : Pos)
           -> (state : CombatState)
           -> Either GameError Event
useSupplyColumn currentSide supportSide units gameMap scLocation state = do
  unit <- findSupportColumn supportSide scLocation units >>= validateSupportFromSC currentSide supportSide state units
  pure $ SupplyColumnUsed supportSide scLocation

resolveCombat :
  (currentSide  : Side) -> (combat : CombatState)
  -> Either GameError Event
resolveCombat currentSide state@(MkCombatState combatHex (MkEngagedUnits atk tac strat) (MkEngagedUnits def tac' strat') losses) =
  let baseOdds = Combat.attack (map fst atk) (map fst def)
      supOdds = Combat.support (map fst tac) (map fst tac') baseOdds
      unmodOdds = odds supOdds
      shiftedOdds = (unmodOdds >>> strat) <<<  strat'
  in Right (CombatResolved state (resolve shiftedOdds 3)) -- need to handle dice rolling => store seed in game state


doLoseStep : (side : Side) -> (units : EngagedUnits) -> (losses : Nat) -> (unitName : String) -> Either GameError (GameUnit, Nat)
doLoseStep side (MkEngagedUnits base _ _) Z unitName = Left $ NoStepsToLose side
doLoseStep side (MkEngagedUnits base _ _) (S k) unitName = do
  case findUnit unitName {positions=base} of
    Nothing => Left $ NoSuchUnits [ unitName ]
    Just (u,p) => Right (u, k)

loseStep : (currentSide : Side) -> (lossSide : Side) -> (combat : CombatState)
         -> (unitName : String)
         -> Either GameError Event
loseStep _ side (MkCombatState _ _ _ Nothing) _ = Left $ NotYourTurn side -- TODO should never happen, error is a placeholder
loseStep currentSide lossSide (MkCombatState _ atk def (Just (al /> dl))) unitName =
  if currentSide == lossSide
  then do
    (u, k) <- doLoseStep lossSide atk al unitName
    pure $ StepLost lossSide u (k /> dl)
  else do
    (u, k) <- doLoseStep lossSide def dl unitName
    pure $ StepLost lossSide u (al /> k)

namespace CombatTest


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

  fail_attack_if_attacked_hex_is_empty : attackWith Axis CombatTest.positions TestMap [ "21/20Pz" ] (Hex 4 5) = Left (NothingToAttack (Hex 4 5))
  fail_attack_if_attacked_hex_is_empty = Refl

  fail_attack_if_attacked_units_Are_of_same_side_than_attacker : attackWith Axis CombatTest.positions TestMap [ "21/20Pz" ] (Hex 3 4) = Left (AttackingOwnUnits [ GameUnit.g59_20pz ] (Hex 3 4))
  fail_attack_if_attacked_units_Are_of_same_side_than_attacker = Refl

  combatState : CombatState
  combatState = MkCombatState (Hex 5 4)
                (MkEngagedUnits [ (GameUnit.g21_20pz, Hex 4 4) ] [] 0)
                (MkEngagedUnits [ (GameUnit.p13_5dp, Hex 5 4) ] [] 0)
                Nothing

  fail_support_if_unit_not_HQ_or_arty :
    supportWith Axis Axis CombatTest.positions TestMap [ "59/20Pz" ] CombatTest.combatState = Left (NotSupportingUnits [ GameUnit.g59_20pz ])
  fail_support_if_unit_not_HQ_or_arty = Refl

  fail_support_if_attacker_hq_not_in_support_range :
    supportWith Axis Axis ((GameUnit.g20pz, Hex 1 1) :: CombatTest.positions) TestMap [ "HQ/20Pz" ] CombatTest.combatState = Left (NotInSupportRange [ GameUnit.g20pz ])
  fail_support_if_attacker_hq_not_in_support_range = Refl

  fail_support_if_hq_cannot_command_unit :
    supportWith Axis Axis ((GameUnit.gBrgPzG, Hex 3 3) :: CombatTest.positions) TestMap [ "HQ/BrgPzG" ] CombatTest.combatState = Left (NotInChainOfCommand [ GameUnit.gBrgPzG ])
  fail_support_if_hq_cannot_command_unit = Refl

  can_support_with_arty_given_its_in_range :
    supportWith Axis Axis ((GameUnit.g777Arty, Hex 1 1) :: CombatTest.positions) TestMap [ "777" ] CombatTest.combatState = Right (TacticalSupportProvided Axis [ (GameUnit.g777Arty, Hex 1 1) ])
  can_support_with_arty_given_its_in_range = Refl

  fail_to_use_supply_column_if_no_sc_at_location :
    useSupplyColumn Axis Axis ((GameUnit.gSupplyColumn, Hex 5 3) :: CombatTest.positions) TestMap (Hex 5 4) CombatTest.combatState = Left (NoSupplyColumnThere (Hex 5 4))
  fail_to_use_supply_column_if_no_sc_at_location = Refl

  fail_to_use_supply_column_if_sc_not_neighbour_to_support_hex :
    useSupplyColumn Axis Axis ((GameUnit.gSupplyColumn, Hex 5 2) :: CombatTest.positions) TestMap (Hex 5 2) CombatTest.combatState = Left (NotInSupportRange [GameUnit.gSupplyColumn])
  fail_to_use_supply_column_if_sc_not_neighbour_to_support_hex = Refl

  fail_to_use_supply_column_if_units_are_not_commanded :
      useSupplyColumn Axis Axis ((GameUnit.gSupplyColumn, Hex 5 3) :: CombatTest.positions) TestMap (Hex 5 3) CombatTest.combatState = Left (NotInChainOfCommand [GameUnit.gSupplyColumn])
  fail_to_use_supply_column_if_units_are_not_commanded = Refl

  use_supply_column_returns_supply_column_used_event :
      useSupplyColumn Axis Axis ((GameUnit.g20pz, Hex 2 2) :: (GameUnit.gSupplyColumn, Hex 4 3) :: CombatTest.positions) TestMap (Hex 4 3) CombatTest.combatState = Right (SupplyColumnUsed Axis (Hex 4 3))
  use_supply_column_returns_supply_column_used_event = Refl

  lossState : CombatState
  lossState = record { losses = Just (1 /> 0) } CombatTest.combatState

  fail_to_lose_step_if_unit_is_not_engaged :
    loseStep Axis Axis CombatTest.lossState "foo" = Left $ NoSuchUnits ["foo"]
  fail_to_lose_step_if_unit_is_not_engaged = Refl

  fail_to_lose_step_if_no_steps_to_lose :
    loseStep Axis Allies CombatTest.lossState "13/5DP" = Left $ NoStepsToLose Allies
  fail_to_lose_step_if_no_steps_to_lose = Refl

  lose_steps_returns_step_loss_event :
    loseStep Axis Axis CombatTest.lossState "21/20Pz" = Right $ StepLost Axis GameUnit.g21_20pz (0 /> 0)
  lose_steps_returns_step_loss_event = Refl
