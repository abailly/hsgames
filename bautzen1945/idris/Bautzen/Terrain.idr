module Bautzen.Terrain

import Bautzen.GameUnit
import Bautzen.Pos as P

import Decidable.Equality

import Data.Fin
import Data.List
import Data.Maybe
import Data.Nat
import Data.Vect
import Data.String

import JSON
import JSON.Parser

%default total

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
Eq Terrain where
  Clear == Clear = True
  Wood == Wood = True
  Rough == Rough = True
  RoughWood == RoughWood = True
  (Hill base) == Hill base' = base == base'
  (Village base) == Village base' = base == base'
  Town == Town = True
  (SupplySource side base) == SupplySource side' base' =
     side == side' && base == base'
  _ == _ = False

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
Eq Connection where
  Plain == Plain = True
  (Road base) == (Road base')  = base == base'
  (River base) == (River base')  =  base == base'
  Lake == Lake = True
  _ == _ = False

public export
Show Connection where
  show Plain       = "Pl"
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
Show Cost where
  show Impossible = "∞"
  show Zero = "0"
  show (Half x) = "(" ++ show x ++ " / 2)"
  show (One x) = "(" ++ show x ++ " + 1)"
  show (Two x) = "(" ++ show x ++ " * 2)"

public export
Eq Cost where
  Impossible == Impossible = True
  Zero == Zero = True
  (Half x) == Half x' = x == x'
  (One x) == One x' = x == x'
  (Two x) == Two x' = x == x'
  _ == _ = False

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
toNat (Half x)   = divNatNZ (toNat x) 2 SIsNonZero
toNat (One x)    = S (toNat x)
toNat (Two x)    = S (S (toNat x))

export
ToJSON Cost where
  toJSON Impossible = object [ ("tag", string "∞") ]
  toJSON Zero = object [("tag", string "0" )]
  toJSON (Half x) = object [ ("tag", string "½"), ("rest", toJSON x) ]
  toJSON (One x) = object [ ("tag", string "1"), ("rest", toJSON x) ]
  toJSON (Two x) = object [ ("tag", string "2"), ("rest", toJSON x) ]


partial
export
FromJSON Cost where
  fromJSON = withObject "Cost" $ \ obj => do
    tag <- field obj "tag"
    case the String tag of
      "∞" => pure Impossible
      "0" => pure Zero
      "½" => Half <$> (field obj "rest")
      "1" => One <$> (field obj "rest")
      "2" => Two <$> (field obj "rest")
      _ => fail #"Invalid tag for Cost: #{tag}"#

||| The actual`Pos` type relevant for this game
public export
record Pos where
  constructor MkPos
  pos : Loc 23 13

mkPosInjective : MkPos p = MkPos p' -> p = p'
mkPosInjective Refl = Refl

public export
hex : Fin 23 -> Fin 13 -> Pos
hex c r = MkPos $ P.Hex c r

export
Show Pos where
  show (MkPos p) = show p

export
Eq Pos where
  MkPos p == MkPos p' = p == p'

export
DecEq Pos where
  decEq (MkPos p) (MkPos p') with (decEq p p')
    decEq (MkPos p) (MkPos p') | (Yes prf) = Yes $ cong MkPos prf
    decEq (MkPos p) (MkPos p') | (No contra) = No $ \ p => contra (mkPosInjective p)

export
Ord Pos where
  compare (MkPos p) (MkPos p') = compare p p'

export
distance : Pos -> Pos -> Nat
distance (MkPos p) (MkPos p') =
  let IsDistance d = P.distance p p' in d

public export
neighbours : Pos -> List Pos
neighbours (MkPos p) = map MkPos $ P.neighbours p

public export
record Map where
  constructor MkMap
  hexes : List (Pos, Terrain)
  edges : List (Pos, List (Pos, Connection))

public export
Eq Map where
  MkMap hexes edges == MkMap hexes' edges'  =
    hexes == hexes' && edges == edges'

public export
tabulate : List (Pos, a) -> Vect 13 (Vect 23 (Maybe a))
tabulate = foldr append table
  where
    append : (Pos, a) -> Vect 13 (Vect 23 (Maybe a)) -> Vect 13 (Vect 23 (Maybe a))
    append (MkPos (P.Hex c r), z) vect =
      let row = index r vect
      in replaceAt r (replaceAt c (Just z) row) vect

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
  MkMap [ (hex 3 4, Wood)
        , (hex 4 4, Clear)
        , (hex 4 5, Hill Rough)
        , (hex 5 4, Clear)
        , (hex 3 5, RoughWood)
        , (hex 2 4, Rough)
        , (hex 2 3, Hill (RoughWood))
        , (hex 3 3, Wood)
        , (hex 4 3, Town)
        , (hex 8 6, Clear)
        , (hex 8 7, Village Clear)
        , (hex 7 7, Hill Rough)
        , (hex 10 2, Village Wood)
        , (hex 10 3, Clear)
        ]
        [ (hex 4 4, [ (hex 5 4, Road Plain) ])
        , (hex 8 6, [ (hex 8 7, Lake) ])
        , (hex 8 7, [ (hex 7 7, Road Plain) ])
        , (hex 10 3, [ (hex 10 2, Road (River Plain)) ])
        ]

public export
allPossibleConnections : List (Pos, List (Pos, Connection))
allPossibleConnections = catMaybes [ mkPosAndNeighbours c r | c <- [0 .. 22], r <- [0 .. 12]]
  where
    mkPosAndNeighbours : (c : Nat) -> (r : Nat) -> Maybe (Pos, List (Pos, Connection))
    mkPosAndNeighbours c r = do
      col <- natToFin c 23
      row <- natToFin r 13
      let pos = hex col row
          cnx = map (\ p => (p,Plain)) (neighbours pos)
      pure $ (pos, cnx)


public export
sameName : String -> (GameUnit, Pos) -> Bool
sameName unitName (u,_) = fullName u == unitName

public export
samePosition : Pos -> (GameUnit, Pos) -> Bool
samePosition to (_,p) = p == to
