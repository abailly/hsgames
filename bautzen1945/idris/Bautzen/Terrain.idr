module Bautzen.Terrain

import Bautzen.GameUnit
import Bautzen.Pos

%access public export
%default total

-- Map & Terrain types

||| Terrain types
data Terrain : Type where
  Clear : Terrain
  Wood : Terrain
  Rough : Terrain
  RoughWood : Terrain
  Hill : (base : Terrain) -> Terrain
  Village : (base : Terrain) -> Terrain
  Town : Terrain
  SupplySource : (base : Terrain) -> Terrain

||| Terrain type between hexes (eg. edges)
data Connection : Type where
  Plain : Connection
  Road : (base : Connection) -> Connection
  River : Connection
  Lake : Connection

data Cost : Type where
  Impossible : Cost
  Zero : Cost
  Half : Cost -> Cost
  One : Cost -> Cost
  Two : Cost -> Cost

cost : UnitType -> Terrain ->          Connection -> Cost
cost   unitType    (SupplySource base) cnx        = cost unitType base cnx
cost   _           _                   Lake       = Impossible
cost   Infantry    terrain             (Road cnx) = Half (cost Infantry terrain cnx)
cost   Infantry    (Hill base)         cnx        = Two (cost Infantry base cnx)
cost   unitType    (Village t)         cnx        = One (cost unitType t cnx)
cost   Infantry    RoughWood cnx                  = One (One Zero)
cost   Infantry    _                   _          = One Zero
cost   unitType    (Hill base)         (Road cnx) = Half (cost unitType base cnx)
cost   _           (Hill _)            _          = Impossible
cost   unitType    RoughWood           cnx        = Two (Two Zero)
cost   unitType    Rough               cnx        = Two Zero
cost   _           Wood                _          = Two Zero
cost   _           _                   _          = One Zero

toNat : Cost -> Nat
toNat Impossible = 10000000 -- should probably be another type?
toNat Zero       = 0
toNat (Half x)   = divNatNZ (toNat x) 2 SIsNotZ
toNat (One x)    = S (toNat x)
toNat (Two x)    = S (S (toNat x))

record Map where
  constructor MkMap
  hexes : List (Pos, Terrain)
  edges : List ((Pos, Pos), Connection)


||| Retrieve the `Terrain`s in a position
terrain : Pos -> Map -> Terrain
terrain pos map =
  case lookup pos (hexes map) of
     Nothing => Clear
     (Just ts) => ts

||| Retrieve the types of connections between 2 hexes
connection : Pos -> Pos -> Map -> Connection
connection x y map =
  case lookup (x,y) (edges map) of
    Nothing => Plain
    Just cs => cs

TestMap : Map
TestMap =
  MkMap [ (Hex 3 4, Wood)
        , (Hex 4 4, Clear)
        , (Hex 4 5, Hill Rough)
        , (Hex 3 5, RoughWood)
        , (Hex 2 4, Rough)
        , (Hex 2 3, Hill (RoughWood))
        , (Hex 3 3, Wood)
        , (Hex 4 3, Town)
        , (Hex 8 6, Clear)
        , (Hex 8 7, Village Clear)
        , (Hex 7 7, Hill Rough)
        , (Hex 10 2, Village Wood)
        , (Hex 10 3, Clear)
        ]
        [ ((Hex 4 4, Hex 5 5), Road Plain)
        , ((Hex 8 6, Hex 8 7), Lake)
        , ((Hex 8 7, Hex 7 7), Road Plain)
        , ((Hex 10 3, Hex 10 2), Road River)
        ]
