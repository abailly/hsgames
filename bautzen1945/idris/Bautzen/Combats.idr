||| Core types and utility functions for all things related to combat
module Bautzen.Combats

import Bautzen.GameUnit
import Bautzen.Terrain

import Data.Fin
import Data.List
import Data.Nat
import Data.Vect

%default total

||| Combat result as steps lost by attacker and defender.
public export
record Losses where
  constructor (/>)

  ||| Steps lost by attacker
  attackerLoss : Nat

  ||| Steps lost by defender. Can be transformed in hexes of retreat.
  defenderLoss : Nat

infix 1 />

public export
Show Losses where
  show (attackerLoss /> defenderLoss) =
    show attackerLoss ++ "/" ++ show defenderLoss

public export
record EngagedUnits where
  constructor MkEngagedUnits
  base : List (GameUnit, Pos)
  tacticalSupport : List (GameUnit, Pos)
  strategicSupport : Nat

public export
Show EngagedUnits where
  show (MkEngagedUnits base tacticalSupport strategicSupport) =
    "MkEngagedUnits base="++ show base ++
    ", tacticalSupport=" ++ show tacticalSupport ++
    ", strategicSupport=" ++ show strategicSupport

public export
record CombatState where
  constructor MkCombatState
  combatHex : Pos
  attackers : EngagedUnits
  defenders : EngagedUnits
  losses : Maybe Losses

public export
Show CombatState where
  show (MkCombatState combatHex attackers defenders losses) =
    "MkCombatState hex=" ++ show combatHex ++
    ", attacker=" ++ show attackers ++
    ", defender="++ show defenders ++
    ", losses=" ++ show losses

||| Combat resolution table.
|||
||| The table is transposed w.r.t. the actual rules booklet
public export
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
public export
reduce : (unit : GameUnit) -> (units : List (GameUnit, Pos)) -> List (GameUnit, Pos)
reduce unit = catMaybes . map (\ (u,p) => reduceUnit u p)
  where
    reduceUnit : GameUnit -> Pos -> Maybe (GameUnit, Pos)
    reduceUnit u@(MkGameUnit nation unitType name parent size move currentMP steps hits combat) pos =
      if u /= unit
      then Just (u, pos)
      else (\ h => (MkGameUnit nation unitType name parent size move currentMP steps h combat, pos)) <$> strengthen (FS hits)
