module Bautzen.Game.Map

import Bautzen.Terrain
import Bautzen.GameUnit
import Bautzen.Pos

import Data.Maybe.Extra

import Data.Nat
import Data.Vect

V : Terrain -> Terrain
V = Village

Wd  : Terrain
Wd = Wood

Cl  : Terrain
Cl = Clear

Rg  : Terrain
Rg = Rough

RW  : Terrain
RW = RoughWood

H : Terrain -> Terrain
H = Hill

SA : Terrain -> Terrain
SA = SupplySource Allies

SX : Terrain -> Terrain
SX = SupplySource Axis

T : Terrain
T = Town

N : Terrain
N = Clear

A : Side
A = Allies

terrains : Vect 13 (Vect 23 Terrain)
terrains = [-- 01      02     03          04    05     06     07     08     09      10     11     12     13     14         15     16     17     18      19     20          21    22      23
 {- 01 -}    [ Cl    , Wd   , Cl        , Wd  , Wd   , V Cl , Cl   , Wd    , Cl   , Wd   , Wd   , Wd   , Wd   , Wd       , Wd   , Wd   , Wd   , Wd    , Cl   , SA (V Wd) , Cl   , Wd   , Cl    ]
 {- 02 -}  , [ Wd    , Wd   , Wd        , Wd  , Wd   , Wd   , Wd   , Wd    , Wd   , V Wd , Wd   , Wd   , Wd   , Wd       , Wd   , Wd   , Wd   , V Wd  , Wd   , V Wd      , V Wd , Wd   , Wd    ]
 {- 03 -}  , [ Wd    , Wd   , Wd        , Wd  , Wd   , Cl   , V Wd , Wd    , Wd   , Cl   , Wd   , Wd   , Wd   , Wd       , Wd   , Wd   , Wd   , Wd    , Wd   , Wd        , Wd   , Wd   , Cl    ]
 {- 04 -}  , [ SX Wd , Cl   , Wd        , Wd  , Wd   , Cl   , V Cl , Cl    , Cl   , V Cl , Cl   , Cl   , V Wd , Wd       , V Wd , Wd   , V Wd , Cl    , Cl   , Cl        , V Cl , Cl   , Cl    ]
 {- 05 -}  , [ Cl    , Wd   , Wd        , Cl  , Cl   , Cl   , Cl   , Cl    , Cl   , Cl   , Cl   , Cl   , Cl   , Cl       , Wd   , Wd   , Wd   , Wd    , V Cl , Cl        , Cl   , Cl   , SA Cl ]
 {- 06 -}  , [ Cl    , Wd   , Wd        , Cl  , Cl   , Cl   , V Cl , Cl    , Cl   , H RW , RW   , Wd   , RW   , H (V RW) , Cl   , V Wd , Wd   , Wd    , V Wd , Wd        , Cl   , T    , SA Cl ]
 {- 07 -}  , [ SX Wd , Wd   , V Wd      , H Rg, Cl   , Cl   , H Rg , T     , Cl   , Cl   , RW   , V Cl , Cl   , V Cl     , Wd   , Cl   , Cl   , Wd    , Cl   , Wd        , V Cl , V Cl , Cl    ]
 {- 08 -}  , [ Wd    , RW   , Wd        , RW  , Cl   , Cl   , V Cl , Rg    , V Cl , Cl   , V Cl , Cl   , Cl   , Cl       , Cl   , Cl   , V Cl , Cl    , Cl   , Cl        , V Cl , Cl   , Cl    ]
 {- 09 -}  , [ Cl    , Rg   , Wd        , Rg  , Cl   , Wd   , Cl   , H RW  , Wd   , V Cl , Cl   , V Cl , Cl   , T        , Cl   , Cl   , Cl   , Cl    , V Cl , Cl        , Cl   , Cl   , Cl    ]
 {- 10 -}  , [ SX Wd , V Cl , V Cl      , Wd  , V Cl , Cl   , Wd   , Cl    , V Rg , Rg   , Wd   , Cl   , Cl   , Cl       , T    , Cl   , Cl   , Cl    , V Cl , Cl        , Cl   , Cl   , Cl    ]
 {- 11 -}  , [ Wd    , V Wd , Cl        , Cl  , Wd   , Cl   , Cl   , Cl    , T    , H Rg , Cl   , RW   , RW   , Rg       , Cl   , H RW , RW   , H Rg  , Cl   , Cl        , Cl   , V Cl , Cl    ]
 {- 12 -}  , [ Wd    , Wd   , V Wd      , Wd  , Cl   , Cl   , Rg   , SX Rg , Rg   , Cl   , RW   , Cl   , Rg   , Cl       , RW   , Cl   , Cl   , SX Cl , T    , Cl        , Cl   , Cl   , Cl    ]
 {- 13 -}  , [ T     , N    , SX (V Cl) , N   , Cl   , N    , Cl   , N     , Cl   , N    , Cl   , N    , Cl   , N        , Cl   , N    , Cl   , N     , Cl   , N         , Cl   , N    , Cl    ]
           ]

public export
PartialGameMap : Map
PartialGameMap = MkMap positions []
  where
    positions : List (Pos, Terrain)
    positions = [(Hex 0 0, Clear),
                 (Hex 0 1, Clear),
                 (Hex 0 2, Clear),
                 (Hex 0 3, SupplySource Axis $ Clear),
                 (Hex 0 4, Clear),
                 (Hex 1 0, Clear),
                 (Hex 1 1, Clear),
                 (Hex 1 2, Clear),
                 (Hex 1 3, Clear),
                 (Hex 1 4, Clear),
                 (Hex 2 0, Clear),
                 (Hex 2 1, Clear),
                 (Hex 2 2, Clear),
                 (Hex 2 3, Clear),
                 (Hex 2 4, Clear),
                 (Hex 3 0, Clear),
                 (Hex 3 1, Clear),
                 (Hex 3 2, Clear),
                 (Hex 3 3, Clear),
                 (Hex 3 4, Clear),
                 (Hex 4 0, Clear),
                 (Hex 4 1, Clear),
                 (Hex 4 2, Clear),
                 (Hex 4 3, Clear),
                 (Hex 4 4, Clear)]

public export
FullGameMap : Map
FullGameMap = MkMap positions []
  where
    mkPosition : (c : Nat) -> (r : Nat) -> Maybe ( Pos, Terrain)
    mkPosition c r with (natToFin c 23, natToFin r 13, isLTE c 22, isLTE r 12)
      mkPosition c r | (Just col, Just row, Yes cprf, Yes rprf) =
           let terrain = index col $ index row terrains
           in Just (Hex c r, terrain)
      mkPosition c r | (_, _, _, _) = Nothing

    positions : List (Pos, Terrain)
    positions = catMaybes $ [ mkPosition c r | c <- [ 0 .. 22 ], r <- [ 0 .. 12 ]]
