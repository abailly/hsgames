||| Rules for Supply
||| section 7 of game rules
module Bautzen.Game.Supply

import Bautzen.GameUnit
import Bautzen.ZoC
import Bautzen.Game.Core
import Bautzen.Game.Move
import Bautzen.Pos
import Bautzen.Terrain

import Data.Heap.LeftistHeap

import Data.SortedMap as SMap

%access export

-- section 7.1.2

||| State for a single step of exploration in an instance of A*
record AState where
  constructor MkAState
  ||| Starting position of this path
  src : Pos

  ||| Target position of this path
  tgt : Pos

  ||| The current accumulated path, in reverse order (eg. from `src` to the
  ||| current explored solution
  path : List Pos

  ||| The accumulated path's cost so far
  costSoFar : Nat

  ||| Currently known costs to reach various positions
  costsMap : SMap.SortedMap Pos Nat

Eq AState where
  (MkAState src tgt path costSoFar _) == (MkAState src' tgt' path' costSoFar' _) =
    src == src' && tgt == tgt' && costSoFar == costSoFar'

Ord AState where
  compare (MkAState src tgt path costSoFar _) (MkAState src' tgt' path' costSoFar' _) =
    case compare costSoFar costSoFar' of
      EQ => case compare tgt tgt' of
        EQ => compare src src'
        ordering => ordering
      ordering => ordering

computeShortestPath : (units : List (GameUnit, Pos)) -> (gameMap : Map) -> (unit : GameUnit) -> BinaryHeap AState -> List Pos
computeShortestPath units gameMap unit state =
  if isEmpty state
  then []
  else let (q, st) = Heap.pop state
       in case st of
            Nothing => computeShortestPath units gameMap unit q
            Just cur@(MkAState src tgt path _ _) =>
              if src == tgt
              then path
              else let q' = foldr (addNeighbours cur) q (neighbours src)
                   in computeShortestPath units gameMap unit q'
  where
    enemyIn : Pos -> Bool
    enemyIn hex = case find (\ (u,p) => p == hex) units of
                    Nothing => False
                    (Just (other, _)) => not (friendly (nation unit) (nation other))

    cannotMoveInto : Pos -> Bool
    cannotMoveInto hex =
      (inZoC (side $ nation unit) units hex /= Free) ||
      enemyIn hex

    addNeighbours : AState -> Pos -> BinaryHeap AState -> BinaryHeap AState
    addNeighbours (MkAState src tgt path costSoFar costsMap) hex queue =
      if cannotMoveInto hex
      then queue
      else case movementCost unit units gameMap src hex False of
             (Left l) => queue
             (Right (x ** _)) =>
               let newCost = costSoFar + toNat x
                   costTox = SMap.lookup hex costsMap
               in case costTox of
                    Just oldCost => if newCost < oldCost
                                    then let costEstimate = newCost + (distance hex tgt)
                                         in Heap.push (MkAState hex tgt (hex :: path) costEstimate (SMap.insert hex newCost costsMap)) queue
                                    else queue
                    Nothing => let costEstimate = newCost + (distance hex tgt)
                               in Heap.push (MkAState hex tgt (hex :: path) costEstimate (SMap.insert hex newCost costsMap)) queue
||| Computes a path of adjacent `Pos`itions `from` to `srcs`.
|||
||| The path must be free of enemy `ZoC`s and enemy `units`. This function
||| uses the well-known _A*_ algorithm to build a path to all the given sources for the
||| given unit. See [path finding](https://www.redblobgames.com/pathfinding/a-star/introduction.html#astar) page
||| for more details.
|||
||| @from the unit and position to start looking from
||| @units all units on the map along with their positions. This is needed to ensure path is
||| free of enemies and ZoCs
||| @gameMap the terrain map
||| @srcs target supply sources, to trace a path to
supplyPathTo : (units : List (GameUnit, Pos)) -> (gameMap : Map) -> (srcs : List Pos) -> (from: (GameUnit, Pos)) -> List (List Pos)
supplyPathTo units gameMap srcs (unit, pos) = supplyPathToAcc startStates
  where
    startState : Pos -> AState
    startState tgt = MkAState pos tgt [] 0 SMap.empty

    startStates : List (BinaryHeap AState)
    startStates = map (\ st => Heap.push st empty) $ map startState srcs

    supplyPathToAcc : List (BinaryHeap AState) -> List (List Pos)
    supplyPathToAcc = map (computeShortestPath units gameMap unit)



||| Check if given `unit` is in supply
||| A unit is in supply if it can trace an enemy and ZoC-free line to
||| a `SupplySource` hex.
||| @unit: the unit to check supply for along with its position
||| @units: position of all units on the `map`
||| @gameMap: the terrain
isInSupply : (unit : (GameUnit, Pos)) -> (units : List (GameUnit, Pos)) -> (gameMap : Map) -> Bool
isInSupply (unit, pos) units gameMap =
  let sources = supplySources (nation unit) gameMap -- get all possible supply sources for unit
  in not (isNil $ supplyPathTo units gameMap sources (unit, pos))
