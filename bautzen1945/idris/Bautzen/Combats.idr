||| Core types and utility functions for all things related to combat
module Bautzen.Combats

import Bautzen.GameUnit
import Bautzen.Pos

import Data.Vect

%access public export
%default total

||| Combat result as steps lost by attacker and defender.
record Losses where
  constructor (/>)

  ||| Steps lost by attacker
  attackerLoss : Nat

  ||| Steps lost by defender. Can be transformed in hexes of retreat.
  defenderLoss : Nat

infix 1 />

Show Losses  where
  show (attackerLoss /> defenderLoss) =
    show attackerLoss ++ "/" ++ show defenderLoss

record EngagedUnits where
  constructor MkEngagedUnits
  base : List (GameUnit, Pos)
  tacticalSupport : List (GameUnit, Pos)
  strategicSupport : Nat

Show EngagedUnits where
  show (MkEngagedUnits base tacticalSupport strategicSupport) =
    "MkEngagedUnits base="++ show base ++
    ", tacticalSupport=" ++ show tacticalSupport ++
    ", strategicSupport=" ++ show strategicSupport

record CombatState where
  constructor MkCombatState
  combatHex : Pos
  attackers : EngagedUnits
  defenders : EngagedUnits
  losses : Maybe Losses

Show CombatState where
  show (MkCombatState combatHex attackers defenders losses) =
    "MkCombatState hex=" ++ show combatHex ++
    ", attacker=" ++ show attackers ++
    ", defender="++ show defenders ++
    ", losses=" ++ show losses

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

||| Reduce given unit by one step, updating its state
reduce : (unit : GameUnit) -> (units : List (GameUnit, Pos)) -> List (GameUnit, Pos)
reduce unit units = ?reduce_rhs
