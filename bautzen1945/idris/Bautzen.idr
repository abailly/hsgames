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
  hit : Bool
  combat : Factors unitType

Eq GameUnit where
  unit == unit' = name unit == name unit'

-- list of existing units

-- Russian/Polish

r13_5dp : GameUnit
r13_5dp = MkGameUnit Russian Infantry "13/5DP" Regiment 6 False (MkStdFactors 3 4)

-- German

g21_20pz : GameUnit
g21_20pz = MkGameUnit German Armored "21/20Pz" Regiment 10 False (MkStdFactors 6 4)

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
                                , (Dec, Inc)
                                ]

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

data Command : (segment : GameSegment) -> Type where
  MoveTo : (unitName : String) -> (to : Pos) -> Command Move

data Event : Type where
  ||| Given unit has moved from some position to some other position
  ||| @from:
  Moved : (unitName : String) -> (from : Pos) -> (to : Pos) -> Event

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
    (Just (MkGameUnit nation _ _ _ _ _ _, _)) => InZoC (side nation)

-- ZoC tests

inZoCTrue : (inZoCOf (Hex 3 3) Axis (Bautzen.r13_5dp, Hex 3 4) = True)
inZoCTrue = Refl

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
applyEvent (Moved unitName from to) (MkGameState turn side segment units) =
  MkGameState turn side segment (setPosition unitName to units)

apply : Event -> Game -> Game
apply event (MkGame events curState) =
  MkGame (event :: events) (applyEvent event curState)

moreMoveTo : (unit : GameUnit) -> (units : List (GameUnit, Pos)) -> (from : Pos) -> (to : Pos) -> Either GameError Event
moreMoveTo unit units from to with (inZoC (side (nation unit)) units from, inZoC (side (nation unit)) units to)
  | (InZoC _, InZoC) = Left (MoveFromZocToZoc unit to)
  | (Free , InZoC _) = ?hole
  | (InZoC _, Free) = ?hole
  | (Free, Free) = ?hole

moveTo : (side : Side) -> (units : List (GameUnit, Pos)) -> (unitName : String) -> (to : Pos) -> Either GameError Event
moveTo sideToPlay units unitName to =
  case find (\ (u,_) => name u == unitName) units of
    Nothing => Left (NoSuchUnit unitName)
    (Just (unit, b)) => if side (nation unit) /= sideToPlay
                        then Left (NotYourTurn (side (nation unit)))
                        else case find (\ (u,p) => p == to) units of
                                  Nothing => moreMoveTo unit units b to
                                  (Just (other, _)) => if friendly (nation unit) (nation other)
                                                       then moreMoveTo unit units b to
                                                       else Left (EnemyInHex other to)

cannot_move_if_unit_does_not_exist : moveTo Allies [ (Bautzen.r13_5dp, Hex 3 4) ] "foo" (Hex 3 5) = Left (NoSuchUnit "foo")
cannot_move_if_unit_does_not_exist = Refl

cannot_move_not_current_side : moveTo Axis [ (Bautzen.r13_5dp, Hex 3 4) ] "13/5DP" (Hex 3 5) = Left (NotYourTurn Allies)
cannot_move_not_current_side = Refl

cannot_move_if_target_hex_is_occupied_by_enemy : moveTo Allies [ (Bautzen.r13_5dp, Hex 3 4), (Bautzen.g21_20pz, Hex 3 5) ] "13/5DP" (Hex 3 5) = Left (EnemyInHex Bautzen.g21_20pz (Hex 3 5))
cannot_move_if_target_hex_is_occupied_by_enemy = Refl

cannot_move_from_zoc_to_zoc : moveTo Allies [ (Bautzen.r13_5dp, Hex 3 4), (Bautzen.g21_20pz, Hex 3 5) ] "13/5DP" (Hex 4 4) = Left (MoveFromZocToZoc Bautzen.r13_5dp (Hex 4 4))
cannot_move_from_zoc_to_zoc = Refl

act : (game : Game) -> Command (curSegment game) -> Either GameError Event
act (MkGame events (MkGameState turn side Move units)) (MoveTo unitName to) = moveTo side units unitName to
