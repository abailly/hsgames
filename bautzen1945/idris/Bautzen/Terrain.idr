module Bautzen.Terrain

import Bautzen.GameUnit
import Bautzen.Pos

import Data.Strings.Extra

import Data.Fin
import Data.List
import Data.Maybe
import Data.Nat
import Data.Vect

-- Map & Terrain types

||| Terrain types
public export
data Terrain : Type where
  Clear : Terrain
  Wood : Terrain
  Rough : Terrain
  RoughWood : Terrain
  Hill : (base : Terrain) -> Terrain
  Village : (base : Terrain) -> Terrain
  Town : Terrain
  SupplySource : (side : Side) -> (base : Terrain) -> Terrain

public export
Show Terrain where
  show Clear                      = "Cl"
  show Wood                       = "Wd"
  show Rough                      = "Rg"
  show RoughWood                  = "RW"
  show (Hill base)                = "H (" ++ show base ++ ")"
  show (Village base)             = "V (" ++ show base ++ ")"
  show Town                       = "T"
  show (SupplySource Axis base)   = "SX (" ++ show base ++ ")"
  show (SupplySource Allies base) = "SA (" ++ show base ++ ")"

public export
isSupplyFor : Nation -> Terrain -> Bool
isSupplyFor nation (SupplySource s _) = side nation == s
isSupplyFor nation (Hill base) = isSupplyFor nation base
isSupplyFor nation (Village base) = isSupplyFor nation base
isSupplyFor  _ _ = False

||| Terrain type between hexes (eg. edges)
public export
data Connection : Type where
  Plain : Connection
  ||| A road or rail connection
  Road : (base : Connection) -> Connection
  River : (base : Connection) -> Connection
  Lake : Connection

public export
Show Connection where
  show Plain       = ""
  show (Road base) = "Rd (" ++ show base ++ ")"
  show (River base)= "Rv (" ++ show base ++ ")"
  show Lake        = "Lk"

public export
data Cost : Type where
  Impossible : Cost
  Zero : Cost
  Half : Cost -> Cost
  One : Cost -> Cost
  Two : Cost -> Cost

public export
cost : UnitType -> Terrain ->            Connection -> Cost
cost   unitType    (SupplySource _ base) cnx        = cost unitType base cnx
cost   _           _                     Lake       = Impossible
cost   unitType    terrain               (River cx) = One (cost unitType terrain cx)
cost   Infantry    terrain               (Road cnx) = Half (cost Infantry terrain cnx)
cost   Infantry    (Hill base)           cnx        = Two (cost Infantry base cnx)
cost   unitType    (Village t)           cnx        = One (cost unitType t cnx)
cost   Infantry    RoughWood cnx                    = One (One Zero)
cost   Infantry    _                     _          = One Zero
cost   unitType    (Hill base)           (Road cnx) = Half (cost unitType base cnx)
cost   _           (Hill _)              _          = Impossible
cost   unitType    terrain               (Road cnx) = Half (cost unitType terrain cnx)
cost   unitType    RoughWood             cnx        = Two (Two Zero)
cost   unitType    Rough                 cnx        = Two Zero
cost   _           Wood                  _          = Two Zero
cost   _           _                     _          = One Zero

public export
toNat : Cost -> Nat
toNat Impossible = 10000000 -- should probably be another type?
toNat Zero       = 0
toNat (Half x)   = divNatNZ (toNat x) 2 SIsNotZ
toNat (One x)    = S (toNat x)
toNat (Two x)    = S (S (toNat x))

public export
record Map where
  constructor MkMap
  hexes : List (Pos, Terrain)
  edges : List (Pos, List (Pos, Connection))

public export
tabulate : List (Pos, a) -> Vect 13 (Vect 23 (Maybe a))
tabulate = foldr append table
  where
    append : (Pos, a) -> Vect 13 (Vect 23 (Maybe a)) -> Vect 13 (Vect 23 (Maybe a))
    append (Hex c r, z) vect with (natToFin c 23, natToFin r 13)
      append (Hex c r, z) vect | (Just c', Just r') = let row = index r' vect
                                                      in replaceAt r' (replaceAt c' (Just z) row) vect
      append (Hex c r, a) vect | (_, _) = vect

    table : Vect 13 (Vect 23 (Maybe a))
    table = replicate 13 (replicate 23 Nothing)

public export
Show Map where
  show (MkMap hexes edges) =
    "Map hexes= " ++ unlines terrain ++ "\n   edges=" ++ unlines connections
    where
      terrain : List String
      terrain = toList $ map show (tabulate hexes)

      connections : List String
      connections = toList $ map show (tabulate edges)

||| Retrieve the `Terrain`s in a position
public export
terrain : Pos -> Map -> Terrain
terrain pos map =
  case lookup pos (hexes map) of
     Nothing => Clear
     (Just ts) => ts

||| Retrieve the types of connections between 2 hexes
public export
connection : Pos -> Pos -> Map -> Connection
connection x y map =
  fromMaybe Plain (lookup x (edges map) >>= lookup y)

||| Get the `Pos`itions of all supply sources for given `Nation` on the map
public export
supplySources : Nation -> Map -> List Pos
supplySources n (MkMap hexes edges) = map fst $ filter (isSupplyFor n . snd) hexes

public export
TestMap : Map
TestMap =
  MkMap [ (Hex 3 4, Wood)
        , (Hex 4 4, Clear)
        , (Hex 4 5, Hill Rough)
        , (Hex 5 4, Clear)
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
        [ (Hex 4 4, [ (Hex 5 4, Road Plain) ])
        , (Hex 8 6, [ (Hex 8 7, Lake) ])
        , (Hex 8 7, [ (Hex 7 7, Road Plain) ])
        , (Hex 10 3, [ (Hex 10 2, Road (River Plain)) ])
        ]
