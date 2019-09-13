module Bautzen

import Data.Fin

%default total

-- Base Types
-- Section 1.1

data Nation : Type where
  German : Nation
  Russian : Nation
  Polish : Nation

data Side : Type where
  Axis : Side
  Allies : Side

Eq Side where
  Axis == Axis = True
  Allies == Allies = True
  _ == _ = False

side : Nation -> Side
side German = Axis
side Polish = Allies
side Russian = Allies

friendly : Nation -> Nation -> Bool
friendly n n' = side n == side n'

data UnitType : Type where
  Armored : UnitType
  HeavyArmored : UnitType
  MechInfantry : UnitType
  Infantry : UnitType
  HeavyEngineer : UnitType
  Artillery : UnitType
  AntiTank : UnitType
  HQ : UnitType
  SupplyColumn : UnitType

data UnitSize : Type where
  Regiment : UnitSize
  Brigade : UnitSize
  Division : UnitSize
  Corps : UnitSize

record StdFactors where
  constructor MkStdFactors
  attack : Nat
  defense : Nat

record Arty where
  constructor MkArty
  support : Nat
  distance : Nat

Factors : UnitType -> Type
Factors Armored = StdFactors
Factors HeavyArmored = StdFactors
Factors MechInfantry = StdFactors
Factors Infantry = StdFactors
Factors HeavyEngineer = StdFactors
Factors Artillery = Arty
Factors AntiTank = StdFactors
Factors HQ = Arty
Factors SupplyColumn = ()

record GameUnit where
  constructor MkGameUnit
  nation : Nation
  unitType : UnitType
  name : String
  size : UnitSize
  move : Nat
  currentMP : Nat
  hit : Bool
  combat : Factors unitType

Eq GameUnit where
  unit == unit' = name unit == name unit'

-- list of existing units

-- Russian/Polish

r13_5dp : GameUnit
r13_5dp = MkGameUnit Russian Infantry "13/5DP" Regiment 6 6 False (MkStdFactors 3 4)

-- German

g21_20pz : GameUnit
g21_20pz = MkGameUnit German Armored "21/20Pz" Regiment 10 10 False (MkStdFactors 6 4)

-- Section 2

-- Positions & Map

||| A position/hex of the game board encoded as a pair of `Nat`
||| with bounds
data Pos : Type where
  Hex : (col : Nat) -> (row : Nat)
      -> { auto cbound : LTE col 22 }
      -> { auto rbound : LTE row 12 }
      -> Pos

Eq Pos where
  (==) (Hex col row) (Hex col' row') = col == col' && row == row'

Show Pos where
  show (Hex c r) = show2Digits  c ++ show2Digits r
    where
      show2Digits : Nat -> String
      show2Digits n =
        if n < 9
        then "0" ++ show (n + 1)
        else show (n + 1)

data Mvmt : Type where
  Dec : Mvmt
  Neut : Mvmt
  Inc : Mvmt

shiftPos : (x : Nat) -> (prf : LTE x bound) -> Mvmt -> Maybe (n : Nat ** LTE n bound)
shiftPos Z prf Dec = Nothing
shiftPos (S k) prf Dec = Just (k ** lteSuccLeft prf)
shiftPos x prf Neut = Just (x ** prf)
shiftPos x prf Inc {bound} with (isLTE (S x) bound)
  | (Yes y) = Just (S x ** y)
  | (No contra) = Nothing


makePos : (pos : Pos) -> (Mvmt, Mvmt) -> Maybe Pos
makePos (Hex col row {cbound} {rbound} ) (a, b) = do
  (c' ** p1) <- shiftPos col cbound a
  (r' ** p2) <- shiftPos row rbound b
  pure $ Hex c' r' {cbound = p1} {rbound = p2}

||| Compute the neighbours of a given position
||| There are at most 6 neighbours, with side and corner hexes having of
||| course less.
neighbours : (pos : Pos) -> List Pos
neighbours pos =
  catMaybes $ map (makePos pos) [ (Inc, Inc)
                                , (Inc, Neut)
                                , (Neut, Inc)
                                , (Neut, Dec)
                                , (Dec, Neut)
                                , (Inc, Dec)
                                ]

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
  SupplySource : Terrain

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

cost : UnitType -> Terrain -> Connection -> Cost
cost _        _            Lake       = Impossible
cost Infantry terrain      (Road cnx) = Half (cost Infantry terrain cnx)
cost Infantry (Hill base)  cnx        = Two (cost Infantry base cnx)
cost unitType (Village t)  cnx        = One (cost unitType t cnx)
cost Infantry RoughWood cnx           = One (One Zero)
cost Infantry _            _          = One Zero
cost unitType (Hill base)  (Road cnx) = Half (cost unitType base cnx)
cost _        (Hill _)     _          = Impossible
cost unitType RoughWood    cnx        = Two (Two Zero)
cost unitType Rough        cnx        = Two Zero
cost _        Wood         _          = Two Zero
cost _        _            _          = One Zero

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

-- Game sequences

data GameSegment : Type where
  Supply : GameSegment
  Move : GameSegment
  Combat : GameSegment

record GameState where
  constructor MkGameState
  turn : Fin 5
  side : Side
  segment : GameSegment
  units : List (GameUnit, Pos)

data GameError : Type where
  NoSuchUnit : (unitName : String) -> GameError
  NotYourTurn : (side : Side) -> GameError
  EnemyInHex : (unit : GameUnit) -> (hex : Pos) -> GameError
  MoveFromZocToZoc : (unit : GameUnit) -> (to : Pos) -> GameError
  ForbiddenTerrain : (from : Pos) -> (to : Pos) -> GameError

data Command : (segment : GameSegment) -> Type where
  MoveTo : (unitName : String) -> (to : Pos) -> Command Move

data Event : Type where
  ||| Unit has moved from some position to some other position
  Moved : (unit : GameUnit) -> (from : Pos) -> (to : Pos) -> (cost : Cost) -> Event

export
data Game : Type where
  MkGame : (events : List Event) -> (curState : GameState) -> Game

curSegment : Game -> GameSegment
curSegment (MkGame events (MkGameState turn side segment units)) = segment

initialState : GameState
initialState = MkGameState 0 Axis Supply []

export
initialGame : Game
initialGame = MkGame [] initialState

-- section 3
-- zones of control

data ZoC : Type where
  InZoC : (side : Side) -> ZoC
  Free : ZoC

||| Test if given position for given `side` is in the ZoC of the unit.
inZoCOf : (pos : Pos) -> (side : Side) -> (GameUnit, Pos) -> Bool
inZoCOf pos curSide (unit, location) with (curSide == side (nation unit))
  | False = pos `elem` neighbours location
  | True = False

||| Is the given `Pos`ition in an enemy ZoC?
||| This assumes the current `side` is playing and checking ZoCs
inZoC : Side -> List (GameUnit, Pos) -> Pos -> ZoC
inZoC curSide units pos =
  case find (inZoCOf pos curSide) units of
    Nothing => Free
    (Just (MkGameUnit nation _ _ _ _ _ _ _, _)) => InZoC (side nation)

-- ZoC tests

inZoCTrue : (inZoCOf (Hex 3 3) Axis (Bautzen.r13_5dp, Hex 3 4) = True)
inZoCTrue = Refl

inZoCTrue2 : (inZoCOf (Hex 4 3) Axis (Bautzen.r13_5dp, Hex 3 4) = True)
inZoCTrue2 = Refl

inZoCFalsePolish : (inZoCOf (Hex 3 3) Allies (Bautzen.r13_5dp, Hex 3 4) = False)
inZoCFalsePolish = Refl

-- section 4
-- Movements

setPosition : String -> Pos -> List (GameUnit, Pos) -> List (GameUnit, Pos)
setPosition unitName newPosition = foldr setPos []
  where
    setPos : (GameUnit, Pos) -> List (GameUnit, Pos) -> List (GameUnit, Pos)
    setPos u@(unit, pos) acc =
      if name unit == unitName
      then (unit, newPosition) :: acc
      else u :: acc

applyEvent : Event -> GameState -> GameState
applyEvent (Moved unit from to _) (MkGameState turn side segment units) =
  MkGameState turn side segment (setPosition (name unit) to units)

apply : Event -> Game -> Game
apply event (MkGame events curState) =
  MkGame (event :: events) (applyEvent event curState)

movementCost : (unit : GameUnit) -> (units : List (GameUnit, Pos)) -> (gameMap : Map) -> (from : Pos) -> (to : Pos) -> Either GameError Cost
movementCost unit units gameMap from to with (cost (unitType unit) (terrain to gameMap) (connection from to gameMap))
    | Impossible = Left (ForbiddenTerrain from to)
    | c = Right c

moreMoveTo : (unit : GameUnit) -> (units : List (GameUnit, Pos)) -> (gameMap : Map) -> (from : Pos) -> (to : Pos) -> Either GameError Event
moreMoveTo unit units gameMap from to with (inZoC (side (nation unit)) units from, inZoC (side (nation unit)) units to)
  | (InZoC _, Free) = do c <- movementCost unit units gameMap from to
                         pure (Moved unit from to (One c))
  | (Free, _) = do c <- movementCost unit units gameMap from to
                   pure (Moved unit from to c)
  | (_, _) = Left (MoveFromZocToZoc unit to)

moveTo : (side : Side) -> (units : List (GameUnit, Pos)) -> Map -> (unitName : String) -> (to : Pos) -> Either GameError Event
moveTo sideToPlay units gameMap unitName to =
  case find (\ (u,_) => name u == unitName) units of
    Nothing => Left (NoSuchUnit unitName)
    (Just (unit, b)) => if side (nation unit) /= sideToPlay
                        then Left (NotYourTurn (side (nation unit)))
                        else case find (\ (u,p) => p == to) units of
                                  Nothing => moreMoveTo unit units gameMap b to
                                  (Just (other, _)) => if friendly (nation unit) (nation other)
                                                       then moreMoveTo unit units gameMap b to
                                                       else Left (EnemyInHex other to)

TestMap : Map
TestMap =
  MkMap [ (Hex 3 4, Wood)
        , (Hex 4 4, Clear)
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

cannot_move_if_unit_does_not_exist : moveTo Allies [ (Bautzen.r13_5dp, Hex 3 4) ] TestMap "foo" (Hex 3 5) = Left (NoSuchUnit "foo")
cannot_move_if_unit_does_not_exist = Refl

cannot_move_not_current_side : moveTo Axis [ (Bautzen.r13_5dp, Hex 3 4) ] TestMap "13/5DP" (Hex 3 5) = Left (NotYourTurn Allies)
cannot_move_not_current_side = Refl

cannot_move_if_target_hex_is_occupied_by_enemy : moveTo Allies [ (Bautzen.r13_5dp, Hex 3 4), (Bautzen.g21_20pz, Hex 3 5) ] TestMap "13/5DP" (Hex 3 5) = Left (EnemyInHex Bautzen.g21_20pz (Hex 3 5))
cannot_move_if_target_hex_is_occupied_by_enemy = Refl

cannot_move_from_zoc_to_zoc : moveTo Allies [ (Bautzen.r13_5dp, Hex 3 4), (Bautzen.g21_20pz, Hex 3 5) ] TestMap "13/5DP" (Hex 4 4) = Left (MoveFromZocToZoc Bautzen.r13_5dp (Hex 4 4))
cannot_move_from_zoc_to_zoc = Refl

moving_into_clear_terrain_costs_1 : moveTo Allies [ (Bautzen.r13_5dp, Hex 3 4) ] TestMap "13/5DP" (Hex 4 4) = Right (Moved Bautzen.r13_5dp (Hex 3 4) (Hex 4 4) (One Zero))
moving_into_clear_terrain_costs_1 = Refl

infantry_moving_into_rough_terrain_costs_1 : moveTo Allies [ (Bautzen.r13_5dp, Hex 3 4) ] TestMap "13/5DP" (Hex 3 3) = Right (Moved Bautzen.r13_5dp (Hex 3 4) (Hex 3 3) (One Zero))
infantry_moving_into_rough_terrain_costs_1 = Refl

non_infantry_moving_into_rough_terrain_costs_2 : moveTo Axis [ (Bautzen.g21_20pz, Hex 3 4) ] TestMap "21/20Pz" (Hex 3 3) = Right (Moved Bautzen.g21_20pz (Hex 3 4) (Hex 3 3) (Two Zero))
non_infantry_moving_into_rough_terrain_costs_2 = Refl

cost_for_rough_is_cumulative : moveTo Allies [ (Bautzen.r13_5dp, Hex 3 4) ] TestMap "13/5DP" (Hex 3 5) = Right (Moved Bautzen.r13_5dp (Hex 3 4) (Hex 3 5) (One (One Zero)))
cost_for_rough_is_cumulative = Refl

armored_cost_for_rough_is_cumulative : moveTo Axis [ (Bautzen.g21_20pz, Hex 3 4) ] TestMap "21/20Pz" (Hex 3 5) = Right (Moved Bautzen.g21_20pz (Hex 3 4) (Hex 3 5) (Two (Two Zero)))
armored_cost_for_rough_is_cumulative = Refl

moving_across_a_lake_is_forbidden : moveTo Axis [ (Bautzen.g21_20pz, Hex 8 6) ] TestMap "21/20Pz" (Hex 8 7) = Left (ForbiddenTerrain (Hex 8 6) (Hex 8 7))
moving_across_a_lake_is_forbidden = Refl

armored_moving_on_hill_is_forbidden : moveTo Axis [ (Bautzen.g21_20pz, Hex 3 4) ] TestMap "21/20Pz" (Hex 2 3) = Left (ForbiddenTerrain (Hex 3 4) (Hex 2 3))
armored_moving_on_hill_is_forbidden = Refl

armored_moving_on_hill_through_road_costs_half : moveTo Axis [ (Bautzen.g21_20pz, Hex 8 7) ] TestMap "21/20Pz" (Hex 7 7) = Right (Moved Bautzen.g21_20pz (Hex 8 7) (Hex 7 7) (Half (Two Zero)))
armored_moving_on_hill_through_road_costs_half = Refl

infantry_moving_through_road_costs_half : moveTo Allies [ (Bautzen.r13_5dp, Hex 8 7) ] TestMap "13/5DP" (Hex 7 7) = Right (Moved Bautzen.r13_5dp (Hex 8 7) (Hex 7 7) (Half (Two (One Zero))))
infantry_moving_through_road_costs_half = Refl

river_adds_one_PM_to_move : moveTo Allies [ (Bautzen.r13_5dp, Hex 10 3) ] TestMap "13/5DP" (Hex 10 2) = Right (Moved Bautzen.r13_5dp (Hex 10 3) (Hex 10 2) (Half (One (One Zero))))
river_adds_one_PM_to_move = Refl

moving_out_of_ZoC_adds_one_PM_to_move : moveTo Allies [ (Bautzen.r13_5dp, Hex 3 4), (Bautzen.g21_20pz, Hex 3 5) ] TestMap "13/5DP" (Hex 2 3) = Right (Moved Bautzen.r13_5dp (Hex 3 4) (Hex 2 3) (One (Two (One (One Zero)))))
moving_out_of_ZoC_adds_one_PM_to_move = Refl

act : (game : Game) -> Command (curSegment game) -> Either GameError Event
act (MkGame events (MkGameState turn side Move units)) (MoveTo unitName to) = moveTo side units TestMap unitName to
