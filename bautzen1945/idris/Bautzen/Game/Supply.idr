||| Rules for Supply
||| section 7 of game rules
module Bautzen.Game.Supply

import Bautzen.GameUnit
import Bautzen.ZoC
import Bautzen.Game.Core
import Bautzen.Game.Move
import Bautzen.Pos
import Bautzen.Terrain

import Data.Heap.RawBinaryHeap as R
import Data.Heap.LeftistHeap as L

import Data.SortedMap as SMap

%access export
%default total

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

Show AState where
  show (MkAState src tgt path costSoFar) = show src ++ ": " ++ show costSoFar

Eq AState where
  (MkAState src tgt path costSoFar) == (MkAState src' tgt' path' costSoFar') =
    src == src' && tgt == tgt' && costSoFar == costSoFar'

Ord AState where
  compare (MkAState src tgt path costSoFar) (MkAState src' tgt' path' costSoFar') =
    case compare costSoFar costSoFar' of
      EQ => case compare tgt tgt' of
        EQ => compare src src'
        ordering => ordering
      ordering => ordering

computeShortestPath : (fuel : Nat) -> (units : List (GameUnit, Pos)) -> (gameMap : Map) -> (unit : GameUnit) ->  (costsMap : SMap.SortedMap Pos Nat) -> L.BinaryHeap AState -> List Pos
computeShortestPath Z _ _ _ _ queue = []
computeShortestPath (S fuel) units gameMap unit costsMap queue =
  if isEmpty queue
  then []
  else let (q, st) = Heap.pop queue
       in case st of
            Nothing => []
            Just cur@(MkAState src tgt path _) =>
              if src == tgt
              then path
              else let (q', costsMap') = foldl (addNeighbours cur) (q, costsMap) (neighbours src)
                   in computeShortestPath fuel units gameMap unit costsMap' q'
  where
    -- TODO factor to Move
    enemyIn : Pos -> Bool
    enemyIn hex = case find (\ (u,p) => p == hex) units of
                    Nothing => False
                    (Just (other, _)) => not (friendly (nation unit) (nation other))

    cannotMoveInto : Pos -> Bool
    cannotMoveInto hex =
      (inZoC (side $ nation unit) units hex /= Free) ||
      enemyIn hex

    explore : Pos -> Nat -> AState -> (L.BinaryHeap AState, SMap.SortedMap Pos Nat) -> (L.BinaryHeap AState, SMap.SortedMap Pos Nat)
    explore hex newCost state@(MkAState src tgt path costSoFar) (queue, costsMap) =
      let costEstimate = newCost + (distance hex tgt)
      in (Heap.push (MkAState hex tgt (hex :: path) costEstimate) queue, SMap.insert hex newCost costsMap)

    addNeighbours : AState -> (L.BinaryHeap AState, SMap.SortedMap Pos Nat) -> Pos -> (L.BinaryHeap AState, SMap.SortedMap Pos Nat)
    addNeighbours state@(MkAState src tgt path costSoFar) (queue, costsMap) hex =
      if cannotMoveInto hex
      then (queue, costsMap)
      else case movementCost unit units gameMap src hex False of
             (Left l) => (queue, costsMap)
             (Right (x ** _)) =>
               let newCost = costSoFar + toNat x
                   costTox = SMap.lookup hex costsMap
               in case costTox of
                    Just oldCost => if newCost < oldCost
                                    then explore hex newCost state (queue, costsMap)
                                    else (queue, costsMap)
                    Nothing => explore hex newCost state (queue, costsMap)



||| Computes a path of adjacent `Pos`itions `from` to one of `srcs`.
|||
||| The path must be free of enemy `ZoC`s and enemy `units`. This function
||| uses the well-known _A*_ algorithm to build a path to _one_ given sources for the
||| given unit. See [path finding](https://www.redblobgames.com/pathfinding/a-star/introduction.html#astar) page
||| for more details.
|||
||| @from the unit and position to start looking from
||| @units all units on the map along with their positions. This is needed to ensure path is
||| free of enemies and ZoCs
||| @gameMap the terrain map
||| @srcs target supply sources, to trace a path to
supplyPathTo : (units : List (GameUnit, Pos)) -> (gameMap : Map) -> (srcs : List Pos) -> (from: (GameUnit, Pos)) -> List Pos
supplyPathTo units gameMap srcs (unit, pos) = supplyPathToAcc startStates

  where
    startState : Pos -> AState
    startState tgt = MkAState pos tgt [pos] 0

    on : (a -> a -> b) -> (c -> a) -> (c -> c -> b)
    on f g x y = f (g x) (g y)

    startStates : List (L.BinaryHeap AState)
    startStates = map (\ st => Heap.push st Heap.empty) $
                  sortBy (compare `on` (Pos.distance pos . tgt)) $
                  map startState srcs

    getFirstNonEmptyPath : List Pos -> L.BinaryHeap AState -> List Pos
    getFirstNonEmptyPath []   start = computeShortestPath 10000 units gameMap unit SMap.empty start
    getFirstNonEmptyPath path _     = path

    supplyPathToAcc : List (L.BinaryHeap AState) -> List Pos
    supplyPathToAcc = foldl getFirstNonEmptyPath []


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

-- TODO handle rules for Supply columns
-- section 7.2

-- TODO handle effects of lack of supply on unit
-- section 7.3

-- TODO handle special case of supply path going through hex in ZoC containing
-- a friendly unit
