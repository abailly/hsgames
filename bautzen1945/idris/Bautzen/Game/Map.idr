module Bautzen.Game.Map

import Bautzen.Terrain
import Bautzen.GameUnit

import Data.Nat
import Data.Vect
import Data.List

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

Rd : Connection -> Connection
Rd = Road

Rv : Connection -> Connection
Rv = Road

Pl : Connection
Pl = Plain

Lk : Connection
Lk = Lake

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

links : List (Pos, List (Pos, Connection))
links = [
  (hex 00 00, [(hex 01 00, Pl), (hex 00 01, Pl)]),
  (hex 00 01, [(hex 00 00, Pl), (hex 01 00, Pl), (hex 01 01, Pl), (hex 00 02, Pl)]),
  (hex 00 02, [(hex 00 01, Pl), (hex 01 01, Rd Pl), (hex 01 02, Pl), (hex 00 03, Rd Pl)]),
  (hex 00 03, [(hex 00 02, Rd Pl), (hex 01 02, Pl), (hex 01 03, Rd Pl), (hex 00 04, Pl)]),
  (hex 00 04, [(hex 00 03, Pl), (hex 01 03, Pl), (hex 01 04, Pl), (hex 00 05, Pl)]),
  (hex 00 05, [(hex 00 04, Pl), (hex 01 04, Pl), (hex 01 05, Pl), (hex 00 06, Pl)]),
  (hex 00 06, [(hex 00 05, Pl), (hex 01 05, Rd Pl), (hex 01 06, Pl), (hex 00 07, Pl)]),
  (hex 00 07, [(hex 00 06, Pl), (hex 01 06, Pl), (hex 01 07, Pl), (hex 00 08, Pl)]),
  (hex 00 08, [(hex 00 07, Pl), (hex 01 07, Pl), (hex 01 08, Pl), (hex 00 09, Pl)]),
  (hex 00 09, [(hex 00 08, Pl), (hex 01 08, Pl), (hex 01 09, Rd Pl), (hex 00 10, Pl)]),
  (hex 00 10, [(hex 00 09, Pl), (hex 01 09, Pl), (hex 01 10, Rd Pl), (hex 00 11, Pl)]),
  (hex 00 11, [(hex 00 10, Pl), (hex 01 10, Rd Pl), (hex 01 11, Rd Pl), (hex 00 12, Pl)]),
  (hex 00 12, [(hex 00 11, Pl), (hex 01 11, Rd Pl), (hex 01 12, Pl)]),
  (hex 01 00, [(hex 00 00, Pl), (hex 02 00, Pl), (hex 02 01, Pl), (hex 01 01, Pl), (hex 00 01, Pl)]),
  (hex 01 01, [(hex 00 01, Pl), (hex 01 00, Pl), (hex 02 01, Rd Pl), (hex 02 02, Pl), (hex 01 02, Pl), (hex 00 02, Rd Pl)]),
  (hex 01 02, [(hex 00 02, Pl), (hex 01 01, Pl), (hex 02 02, Pl), (hex 02 03, Pl), (hex 01 03, Pl), (hex 00 03, Pl)]),
  (hex 01 03, [(hex 00 03, Rd Pl), (hex 01 02, Pl), (hex 02 03, Rd Pl), (hex 02 04, Pl), (hex 01 04, Pl), (hex 00 04, Pl)]),
  (hex 01 04, [(hex 00 04, Pl), (hex 01 03, Pl), (hex 02 04, Pl), (hex 02 05, Pl), (hex 01 05, Pl), (hex 00 05, Pl)]),
  (hex 01 05, [(hex 00 05, Pl), (hex 01 04, Pl), (hex 02 05, Pl), (hex 02 06, Rd Pl), (hex 01 06, Pl), (hex 00 06, Rd Pl)]),
  (hex 01 06, [(hex 00 06, Pl), (hex 01 05, Pl), (hex 02 06, Pl), (hex 02 07, Pl), (hex 01 07, Pl), (hex 00 07, Pl)]),
  (hex 01 07, [(hex 00 07, Pl), (hex 01 06, Pl), (hex 02 07, Pl), (hex 02 08, Pl), (hex 01 08, Pl), (hex 00 08, Pl)]),
  (hex 01 08, [(hex 00 08, Pl), (hex 01 07, Pl), (hex 02 08, Pl), (hex 02 09, Pl), (hex 01 09, Pl), (hex 00 09, Pl)]),
  (hex 01 09, [(hex 00 09, Rd Pl), (hex 01 08, Pl), (hex 02 09, Rd Pl), (hex 02 10, Pl), (hex 01 10, Rd Pl), (hex 00 10, Pl)]),
  (hex 01 10, [(hex 00 10, Rd Pl), (hex 01 09, Rd Pl), (hex 02 10, Pl), (hex 02 11, Rd Pl), (hex 01 11, Pl), (hex 00 11, Rd Pl)]),
  (hex 01 11, [(hex 00 11, Rd Pl), (hex 01 10, Pl), (hex 02 11, Pl), (hex 02 12, Pl), (hex 01 12, Pl), (hex 00 12, Rd Pl)]),
  (hex 02 00, [(hex 03 00, Pl), (hex 02 01, Rd Pl), (hex 01 00, Pl)]),
  (hex 02 01, [(hex 01 00, Pl), (hex 02 00, Rd Pl), (hex 03 00, Rd Pl), (hex 03 01, Pl), (hex 02 02, Rd Pl), (hex 01 01, Rd Pl)]),
  (hex 02 02, [(hex 01 01, Pl), (hex 02 01, Rd Pl), (hex 03 01, Pl), (hex 03 02, Rd Pl), (hex 02 03, Pl), (hex 01 02, Pl)]),
  (hex 02 03, [(hex 01 02, Pl), (hex 02 02, Pl), (hex 03 02, Rd Pl), (hex 03 03, Rd Pl), (hex 02 04, Pl), (hex 01 03, Rd Pl)]),
  (hex 02 04, [(hex 01 03, Pl), (hex 02 03, Pl), (hex 03 03, Pl), (hex 03 04, Pl), (hex 02 05, Pl), (hex 01 04, Pl)]),
  (hex 02 05, [(hex 01 04, Pl), (hex 02 04, Pl), (hex 03 04, Rd Pl), (hex 03 05, Pl), (hex 02 06, Rd Pl), (hex 01 05, Pl)]),
  (hex 02 06, [(hex 01 05, Rd Pl), (hex 02 05, Rd Pl), (hex 03 05, Rd Pl), (hex 03 06, Pl), (hex 02 07, Rd Pl), (hex 01 06, Pl)]),
  (hex 02 07, [(hex 01 06, Pl), (hex 02 06, Rd Pl), (hex 03 06, Pl), (hex 03 07, Pl), (hex 02 08, Rd Pl), (hex 01 07, Pl)]),
  (hex 02 08, [(hex 01 07, Pl), (hex 02 07, Rd Pl), (hex 03 07, Pl), (hex 03 08, Pl), (hex 02 09, Rd Pl), (hex 01 08, Pl)]),
  (hex 02 09, [(hex 01 08, Pl), (hex 02 08, Rd Pl), (hex 03 08, Pl), (hex 03 09, Rd Pl), (hex 02 10, Pl), (hex 01 09, Rd Pl)]),
  (hex 02 10, [(hex 01 09, Pl), (hex 02 09, Pl), (hex 03 09, Pl), (hex 03 10, Pl), (hex 02 11, Pl), (hex 01 10, Pl)]),
  (hex 02 11, [(hex 01 10, Rd Pl), (hex 02 10, Pl), (hex 03 10, Rd Pl), (hex 03 11, Pl), (hex 02 12, Rd Pl), (hex 01 11, Pl)]),
  (hex 02 12, [(hex 01 11, Pl), (hex 02 11, Rd Pl), (hex 03 11, Pl), (hex 03 12, Pl), (hex 01 12, Pl)]),
  (hex 03 00, [(hex 02 00, Pl), (hex 04 00, Pl), (hex 04 01, Rd Pl), (hex 03 01, Pl), (hex 02 01, Rd Pl)]),
  (hex 03 01, [(hex 02 01, Pl), (hex 03 00, Pl), (hex 04 01, Pl), (hex 04 02, Pl), (hex 03 02, Pl), (hex 02 02, Pl)]),
  (hex 03 02, [(hex 02 02, Rd Pl), (hex 03 01, Pl), (hex 04 02, Pl), (hex 04 03, Rd Pl), (hex 03 03,Rd Pl), (hex 02 03, Rd Pl)]),
  (hex 03 03, [(hex 02 03, Rd Pl), (hex 03 02, Rd Pl), (hex 04 03, Pl), (hex 04 04, Rd Pl), (hex 03 04, Rd Pl), (hex 02 04, Pl)]),
  (hex 03 04, [(hex 02 04, Pl), (hex 03 03, Rd Pl), (hex 04 04, Rd Pl), (hex 04 05, Rd Pl), (hex 03 05, Pl), (hex 02 05, Rd Pl)]),
  (hex 03 05, [(hex 02 05, Pl), (hex 03 04, Pl), (hex 04 05, Pl), (hex 04 06, Rd Pl), (hex 03 06, Pl), (hex 02 06, Rd Pl)]),
  (hex 03 06, [(hex 02 06, Pl), (hex 03 05, Pl), (hex 04 06, Pl), (hex 04 07, Pl), (hex 03 07, Pl), (hex 02 07, Pl)]),
  (hex 03 07, [(hex 02 07, Pl), (hex 03 06, Pl), (hex 04 07, Pl), (hex 04 08, Pl), (hex 03 08, Pl), (hex 02 08, Pl)]),
  (hex 03 08, [(hex 02 08, Pl), (hex 03 07, Pl), (hex 04 08, Pl), (hex 04 09, Pl), (hex 03 09, Pl), (hex 02 09, Pl)]),
  (hex 03 09, [(hex 02 09, Rd Pl), (hex 03 08, Pl), (hex 04 09, Rd Pl), (hex 04 10, Pl), (hex 03 10, Rd Pl), (hex 02 10, Pl)]),
  (hex 03 10, [(hex 02 10, Pl), (hex 03 09, Rd Pl), (hex 04 10, Pl), (hex 04 11, Rd Pl), (hex 03 11, Pl), (hex 02 11, Rd Pl)]),
  (hex 03 11, [(hex 02 11, Pl), (hex 03 10, Pl), (hex 04 11, Pl), (hex 04 12, Pl), (hex 03 12, Pl), (hex 02 12, Pl)]),
  (hex 04 00, [(hex 05 00, Pl), (hex 04 01, Pl), (hex 03 00, Pl)]),
  (hex 04 01, [(hex 03 00, Rd Pl), (hex 04 00, Pl), (hex 05 00, Rd Pl), (hex 05 01, Pl), (hex 04 02, Pl), (hex 03 01, Pl)]),
  (hex 04 02, [(hex 03 01, Pl), (hex 04 01, Pl), (hex 05 01, Pl), (hex 05 02, Pl), (hex 04 03, Pl), (hex 03 02, Pl)]),
  (hex 04 03, [(hex 03 02, Rd Pl), (hex 04 02, Pl), (hex 05 02, Rd Pl), (hex 05 03, Pl), (hex 04 04, Pl), (hex 03 03, Pl)]),
  (hex 04 04, [(hex 03 03, Rd Pl), (hex 04 03, Pl), (hex 05 03, Rd Pl), (hex 05 04, Rd Pl), (hex 04 05, Pl), (hex 03 04, Rd Pl)]),
  (hex 04 05, [(hex 03 04, Rd Pl), (hex 04 04, Pl), (hex 05 04, Pl), (hex 05 05, Pl), (hex 04 06, Rd Pl), (hex 03 05, Pl)]),
  (hex 04 06, [(hex 03 05, Rd Pl), (hex 04 05, Rd Pl), (hex 05 05, Rd Pl), (hex 05 06, Pl), (hex 04 07, Rd Pl), (hex 03 06, Pl)]),
  (hex 04 07, [(hex 03 06, Pl), (hex 04 06, Rd Pl), (hex 05 06, Pl), (hex 05 07, Rd Pl), (hex 04 08, Pl), (hex 03 07, Pl)]),
  (hex 04 08, [(hex 03 07, Pl), (hex 04 07, Pl), (hex 05 07, Pl), (hex 05 08, Pl), (hex 04 09, Pl), (hex 03 08, Pl)]),
  (hex 04 09, [(hex 03 08, Pl), (hex 04 08, Pl), (hex 05 08, Rd Pl), (hex 05 09, Pl), (hex 04 10, Pl), (hex 03 09, Rd Pl)]),
  (hex 04 10, [(hex 03 09, Pl), (hex 04 09, Pl), (hex 05 09, Pl), (hex 05 10, Pl), (hex 04 11, Pl), (hex 03 10, Pl)]),
  (hex 04 11, [(hex 03 10, Rd Pl), (hex 04 10, Pl), (hex 05 10, Rd Pl), (hex 05 11, Pl), (hex 04 12, Pl), (hex 03 11, Pl)]),
  (hex 04 12, [(hex 03 11, Pl), (hex 04 11, Pl), (hex 05 11, Rd Pl), (hex 05 12, Pl), (hex 03 12, Pl)]),
  (hex 05 00, [(hex 04 00, Pl), (hex 06 00, Pl), (hex 06 01, Rd Pl), (hex 05 01, Pl), (hex 04 01, Rd Pl)]),
  (hex 05 01, [(hex 04 01, Pl), (hex 05 00, Rd Pl), (hex 06 01, Pl), (hex 06 02, Pl), (hex 05 02, Rd Pl), (hex 04 02, Pl)]),
  (hex 05 02, [(hex 04 02, Pl), (hex 05 01, Rd Pl), (hex 06 02, Pl), (hex 06 03, Rd Pl), (hex 05 03, Rd Pl), (hex 04 03, Rd Pl)]),
  (hex 05 03, [(hex 04 03, Pl), (hex 05 02, Rd Pl), (hex 06 03, Rd Pl), (hex 06 04, Rd Pl), (hex 05 04, Pl), (hex 04 04, Rd Pl)]),
  (hex 05 04, [(hex 04 04, Rd Pl), (hex 05 03, Pl), (hex 06 04, Pl), (hex 06 05, Pl), (hex 05 05, Rd Pl), (hex 04 05, Pl)]),
  (hex 05 05, [(hex 04 05, Pl), (hex 05 04, Rd Pl), (hex 06 05, Pl), (hex 06 06, Rd Pl), (hex 05 06, Pl), (hex 04 06, Rd Pl)]),
  (hex 05 06, [(hex 04 06, Pl), (hex 05 05, Pl), (hex 06 06, Pl), (hex 06 07, Pl), (hex 05 07, Pl), (hex 04 07, Pl)]),
  (hex 05 07, [(hex 04 07, Rd Pl), (hex 05 06, Pl), (hex 06 07, Rd Pl), (hex 06 08, Pl), (hex 05 08, Rd Pl), (hex 04 08, Pl)]),
  (hex 05 08, [(hex 04 08, Pl), (hex 05 07, Rd Pl), (hex 06 08, Pl), (hex 06 09, Pl), (hex 05 09, Pl), (hex 04 09, Rd Pl)]),
  (hex 05 09, [(hex 04 09, Pl), (hex 05 08, Pl), (hex 06 09, Pl), (hex 06 10, Pl), (hex 05 10, Pl), (hex 04 10, Pl)]),
  (hex 05 10, [(hex 04 10, Pl), (hex 05 09, Pl), (hex 06 10, Pl), (hex 06 11, Rd Pl), (hex 05 11, Pl), (hex 04 11, Rd Pl)]),
  (hex 05 11, [(hex 04 11, Pl), (hex 05 10, Pl), (hex 06 11, Rd Pl), (hex 06 12, Pl), (hex 05 12, Pl), (hex 04 12, Rd Pl)]),
  (hex 06 00, [(hex 07 00, Rv Pl), (hex 06 01, Pl), (hex 05 00, Pl)]),
  (hex 06 01, [(hex 05 00, Rd Pl), (hex 06 00, Pl), (hex 07 00, Rv Pl), (hex 07 01, Rv (Rd Pl)), (hex 06 02, Pl), (hex 05 01, Pl)]),
  (hex 06 02, [(hex 05 01, Pl), (hex 06 01, Pl), (hex 07 01, Rv Pl), (hex 07 02, Pl), (hex 06 03, Pl), (hex 05 02, Pl)]),
  (hex 06 03, [(hex 05 02, Rd Pl), (hex 06 02, Pl), (hex 07 02, Rd Pl), (hex 07 03, Rd Pl), (hex 06 04, Pl), (hex 05 03, Rd Pl)]),
  (hex 06 04, [(hex 05 03, Rd Pl), (hex 06 03, Pl), (hex 07 03, Pl), (hex 07 04, Pl), (hex 06 05, Rd Pl), (hex 05 04, Pl)]),
  (hex 06 05, [(hex 05 04, Pl), (hex 06 04, Rd Pl), (hex 07 04, Pl), (hex 07 05, Pl), (hex 06 06, Rd Pl), (hex 05 05, Pl)]),
  (hex 06 06, [(hex 05 05, Rd Pl), (hex 06 05, Rd Pl), (hex 07 05, Rd Pl), (hex 07 06, Rd Pl), (hex 06 07, Rd Pl), (hex 05 06, Pl)]),
  (hex 06 07, [(hex 05 06, Pl), (hex 06 06, Rd Pl), (hex 07 06, Pl), (hex 07 07, Pl), (hex 06 08, Pl), (hex 05 07, Rd Pl)]),
  (hex 06 08, [(hex 05 07, Pl), (hex 06 07, Pl), (hex 07 07, Pl), (hex 07 08, Pl), (hex 06 09, Pl), (hex 05 08, Pl)]),
  (hex 06 09, [(hex 05 08, Pl), (hex 06 08, Pl), (hex 07 08, Pl), (hex 07 09, Pl), (hex 06 10, Pl), (hex 05 09, Pl)]),
  (hex 06 10, [(hex 05 09, Pl), (hex 06 09, Pl), (hex 07 09, Pl), (hex 07 10, Pl), (hex 06 11, Pl), (hex 05 10, Pl)]),
  (hex 06 11, [(hex 05 10, Rd Pl), (hex 06 10, Pl), (hex 07 10, Rd Pl), (hex 07 11, Pl), (hex 06 12, Pl), (hex 05 11, Rd Pl)]),
  (hex 06 12, [(hex 05 11, Pl), (hex 06 11, Pl), (hex 07 11, Pl), (hex 07 12, Pl), (hex 05 12, Pl)]),
  (hex 07 00, [(hex 06 00, Rv Pl), (hex 08 00, Pl), (hex 08 01, Pl), (hex 07 01, Pl), (hex 06 01, Rv Pl)]),
  (hex 07 01, [(hex 06 01, Rd (Rv Pl)), (hex 07 00, Pl), (hex 08 01, Pl), (hex 08 02, Rd Pl), (hex 07 02, Rv Pl), (hex 06 02, Rv Pl)]),
  (hex 07 02, [(hex 06 02, Pl), (hex 07 01, Rv Pl), (hex 08 02, Rd (Rv Pl)), (hex 08 03, Pl), (hex 07 03, Pl), (hex 06 03, Rd Pl)]),
  (hex 07 03, [(hex 06 03, Rd Pl), (hex 07 02, Pl), (hex 08 03, Pl), (hex 08 04, Rd Pl), (hex 07 04, Pl), (hex 06 04, Pl)]),
  (hex 07 04, [(hex 06 04, Pl), (hex 07 03, Pl), (hex 08 04, Rd Pl), (hex 08 05, Pl), (hex 07 05, Rd Pl), (hex 06 05, Pl)]),
  (hex 07 05, [(hex 06 05, Pl), (hex 07 04, Rd Pl), (hex 08 05, Pl), (hex 08 06, Rd Pl), (hex 07 06, Lk), (hex 06 06, Rd Pl)]),
  (hex 07 06, [(hex 06 06, Rd Pl), (hex 07 05, Lk), (hex 08 06, Pl), (hex 08 07, Rd Pl), (hex 07 07, Rd Pl), (hex 06 07, Pl)]),
  (hex 07 07, [(hex 06 07, Pl), (hex 07 06, Rd Pl), (hex 08 07, Pl), (hex 08 08, Pl), (hex 07 08, Rd Pl), (hex 06 08, Pl)]),
  (hex 07 08, [(hex 06 08, Pl), (hex 07 07, Rd Pl), (hex 08 08, Pl), (hex 08 09, Rd Pl), (hex 07 09, Rd Pl), (hex 06 09, Pl)]),
  (hex 07 09, [(hex 06 09, Pl), (hex 07 08, Rd Pl), (hex 08 09, Pl), (hex 08 10, Rd Pl), (hex 07 10, Pl), (hex 06 10, Pl)]),
  (hex 07 10, [(hex 06 10, Pl), (hex 07 09, Pl), (hex 08 10, Rd Pl), (hex 08 11, Pl), (hex 07 11, Pl), (hex 06 11, Rd Pl)]),
  (hex 07 11, [(hex 06 11, Pl), (hex 07 10, Pl), (hex 08 11, Rd Pl), (hex 08 12, Pl), (hex 07 12, Pl), (hex 06 12, Pl)]),
  (hex 08 00, [(hex 09 00, Pl), (hex 08 01, Pl), (hex 07 00, Pl)]),
  (hex 08 01, [(hex 07 00, Pl), (hex 08 00, Pl), (hex 09 00, Pl), (hex 09 01, Pl), (hex 08 02, Pl), (hex 07 01, Pl)]),
  (hex 08 02, [(hex 07 01, Rd Pl), (hex 08 01, Pl), (hex 09 01, Rd Pl), (hex 09 02, Pl), (hex 08 03, Rv Pl), (hex 07 02, Rv (Rd Pl))]),
  (hex 08 03, [(hex 07 02, Pl), (hex 08 02, Rv Pl), (hex 09 02, Pl), (hex 09 03, Rd Pl), (hex 08 04, Rd Pl), (hex 07 03, Pl)]),
  (hex 08 04, [(hex 07 03, Rd Pl), (hex 08 03, Rd Pl), (hex 09 03, Pl), (hex 09 04, Pl), (hex 08 05, Lk), (hex 07 04, Rd Pl)]),
  (hex 08 05, [(hex 07 04, Pl), (hex 08 04, Lk), (hex 09 04, Rd Pl), (hex 09 05, Rd Pl), (hex 08 06, Rd Pl), (hex 07 05, Pl)]),
  (hex 08 06, [(hex 07 05, Rd Pl), (hex 08 05, Rd Pl), (hex 09 05, Pl), (hex 09 06, Pl), (hex 08 07, Pl), (hex 07 06, Pl)]),
  (hex 08 07, [(hex 07 06, Rd Pl), (hex 08 06, Pl), (hex 09 06, Rd Pl), (hex 09 07, Rd Pl), (hex 08 08, Pl), (hex 07 07, Pl)]),
  (hex 08 08, [(hex 07 07, Pl), (hex 08 07, Pl), (hex 09 07, Pl), (hex 09 08, Pl), (hex 08 09, Pl), (hex 07 08, Pl)]),
  (hex 08 09, [(hex 07 08, Rd Pl), (hex 08 08, Pl), (hex 09 08, Pl), (hex 09 09, Pl), (hex 08 10, Rd Pl), (hex 07 09, Pl)]),
  (hex 08 10, [(hex 07 09, Rd Pl), (hex 08 09, Rd Pl), (hex 09 09, Rd Pl), (hex 09 10, Pl), (hex 08 11, Rd Pl), (hex 07 10, Rd Pl)]),
  (hex 08 11, [(hex 07 10, Pl), (hex 08 10, Rd Pl), (hex 09 10, Pl), (hex 09 11, Pl), (hex 08 12, Pl), (hex 07 11, Rd Pl)]),
  (hex 08 12, [(hex 07 11, Pl), (hex 08 11, Pl), (hex 09 11, Pl), (hex 09 12, Pl), (hex 07 12, Pl)]),
  --
  (hex 09 00, [(hex 08 00, Pl), (hex 10 00, Pl), (hex 10 01, Pl), (hex 09 01, Pl), (hex 08 01, Pl)]),
  (hex 09 01, [(hex 08 01, Pl), (hex 09 00, Pl), (hex 10 01, Pl), (hex 10 02, Pl), (hex 09 02, Pl), (hex 08 02, Pl)]),
  (hex 09 02, [(hex 08 02, Pl), (hex 09 01, Pl), (hex 10 02, Pl), (hex 10 03, Pl), (hex 09 03, Pl), (hex 08 03, Pl)]),
  (hex 09 03, [(hex 08 03, Pl), (hex 09 02, Pl), (hex 10 03, Pl), (hex 10 04, Pl), (hex 09 04, Pl), (hex 08 04, Pl)]),
  (hex 09 04, [(hex 08 04, Pl), (hex 09 03, Pl), (hex 10 04, Pl), (hex 10 05, Pl), (hex 09 05, Pl), (hex 08 05, Pl)]),
  (hex 09 05, [(hex 08 05, Pl), (hex 09 04, Pl), (hex 10 05, Pl), (hex 10 06, Pl), (hex 09 06, Pl), (hex 08 06, Pl)]),
  (hex 09 06, [(hex 08 06, Pl), (hex 09 05, Pl), (hex 10 06, Pl), (hex 10 07, Pl), (hex 09 07, Pl), (hex 08 07, Pl)]),
  (hex 09 07, [(hex 08 07, Pl), (hex 09 06, Pl), (hex 10 07, Pl), (hex 10 08, Pl), (hex 09 08, Pl), (hex 08 08, Pl)]),
  (hex 09 08, [(hex 08 08, Pl), (hex 09 07, Pl), (hex 10 08, Pl), (hex 10 09, Pl), (hex 09 09, Pl), (hex 08 09, Pl)]),
  (hex 09 09, [(hex 08 09, Pl), (hex 09 08, Pl), (hex 10 09, Pl), (hex 10 10, Pl), (hex 09 10, Pl), (hex 08 10, Pl)]),
  (hex 09 10, [(hex 08 10, Pl), (hex 09 09, Pl), (hex 10 10, Pl), (hex 10 11, Pl), (hex 09 11, Pl), (hex 08 11, Pl)]),
  (hex 09 11, [(hex 08 11, Pl), (hex 09 10, Pl), (hex 10 11, Pl), (hex 10 12, Pl), (hex 09 12, Pl), (hex 08 12, Pl)]),
  (hex 09 12, [(hex 08 12, Pl), (hex 09 11, Pl), (hex 10 12, Pl)]),
  (hex 10 00, [(hex 11 00, Pl), (hex 10 01, Pl), (hex 09 00, Pl)]),
  (hex 10 01, [(hex 09 00, Pl), (hex 10 00, Pl), (hex 11 00, Pl), (hex 11 01, Pl), (hex 10 02, Pl), (hex 09 01, Pl)]),
  (hex 10 02, [(hex 09 01, Pl), (hex 10 01, Pl), (hex 11 01, Pl), (hex 11 02, Pl), (hex 10 03, Pl), (hex 09 02, Pl)]),
  (hex 10 03, [(hex 09 02, Pl), (hex 10 02, Pl), (hex 11 02, Pl), (hex 11 03, Pl), (hex 10 04, Pl), (hex 09 03, Pl)]),
  (hex 10 04, [(hex 09 03, Pl), (hex 10 03, Pl), (hex 11 03, Pl), (hex 11 04, Pl), (hex 10 05, Pl), (hex 09 04, Pl)]),
  (hex 10 05, [(hex 09 04, Pl), (hex 10 04, Pl), (hex 11 04, Pl), (hex 11 05, Pl), (hex 10 06, Pl), (hex 09 05, Pl)]),
  (hex 10 06, [(hex 09 05, Pl), (hex 10 05, Pl), (hex 11 05, Pl), (hex 11 06, Pl), (hex 10 07, Pl), (hex 09 06, Pl)]),
  (hex 10 07, [(hex 09 06, Pl), (hex 10 06, Pl), (hex 11 06, Pl), (hex 11 07, Pl), (hex 10 08, Pl), (hex 09 07, Pl)]),
  (hex 10 08, [(hex 09 07, Pl), (hex 10 07, Pl), (hex 11 07, Pl), (hex 11 08, Pl), (hex 10 09, Pl), (hex 09 08, Pl)]),
  (hex 10 09, [(hex 09 08, Pl), (hex 10 08, Pl), (hex 11 08, Pl), (hex 11 09, Pl), (hex 10 10, Pl), (hex 09 09, Pl)]),
  (hex 10 10, [(hex 09 09, Pl), (hex 10 09, Pl), (hex 11 09, Pl), (hex 11 10, Pl), (hex 10 11, Pl), (hex 09 10, Pl)]),
  (hex 10 11, [(hex 09 10, Pl), (hex 10 10, Pl), (hex 11 10, Pl), (hex 11 11, Pl), (hex 10 12, Pl), (hex 09 11, Pl)]),
  (hex 10 12, [(hex 09 11, Pl), (hex 10 11, Pl), (hex 11 11, Pl), (hex 11 12, Pl), (hex 09 12, Pl)]),
  (hex 11 00, [(hex 10 00, Pl), (hex 12 00, Pl), (hex 12 01, Pl), (hex 11 01, Pl), (hex 10 01, Pl)]),
  (hex 11 01, [(hex 10 01, Pl), (hex 11 00, Pl), (hex 12 01, Pl), (hex 12 02, Pl), (hex 11 02, Pl), (hex 10 02, Pl)]),
  (hex 11 02, [(hex 10 02, Pl), (hex 11 01, Pl), (hex 12 02, Pl), (hex 12 03, Pl), (hex 11 03, Pl), (hex 10 03, Pl)]),
  (hex 11 03, [(hex 10 03, Pl), (hex 11 02, Pl), (hex 12 03, Pl), (hex 12 04, Pl), (hex 11 04, Pl), (hex 10 04, Pl)]),
  (hex 11 04, [(hex 10 04, Pl), (hex 11 03, Pl), (hex 12 04, Pl), (hex 12 05, Pl), (hex 11 05, Pl), (hex 10 05, Pl)]),
  (hex 11 05, [(hex 10 05, Pl), (hex 11 04, Pl), (hex 12 05, Pl), (hex 12 06, Pl), (hex 11 06, Pl), (hex 10 06, Pl)]),
  (hex 11 06, [(hex 10 06, Pl), (hex 11 05, Pl), (hex 12 06, Pl), (hex 12 07, Pl), (hex 11 07, Pl), (hex 10 07, Pl)]),
  (hex 11 07, [(hex 10 07, Pl), (hex 11 06, Pl), (hex 12 07, Pl), (hex 12 08, Pl), (hex 11 08, Pl), (hex 10 08, Pl)]),
  (hex 11 08, [(hex 10 08, Pl), (hex 11 07, Pl), (hex 12 08, Pl), (hex 12 09, Pl), (hex 11 09, Pl), (hex 10 09, Pl)]),
  (hex 11 09, [(hex 10 09, Pl), (hex 11 08, Pl), (hex 12 09, Pl), (hex 12 10, Pl), (hex 11 10, Pl), (hex 10 10, Pl)]),
  (hex 11 10, [(hex 10 10, Pl), (hex 11 09, Pl), (hex 12 10, Pl), (hex 12 11, Pl), (hex 11 11, Pl), (hex 10 11, Pl)]),
  (hex 11 11, [(hex 10 11, Pl), (hex 11 10, Pl), (hex 12 11, Pl), (hex 12 12, Pl), (hex 11 12, Pl), (hex 10 12, Pl)]),
  (hex 11 12, [(hex 10 12, Pl), (hex 11 11, Pl), (hex 12 12, Pl)]),
  (hex 12 00, [(hex 13 00, Pl), (hex 12 01, Pl), (hex 11 00, Pl)]),
  (hex 12 01, [(hex 11 00, Pl), (hex 12 00, Pl), (hex 13 00, Pl), (hex 13 01, Pl), (hex 12 02, Pl), (hex 11 01, Pl)]),
  (hex 12 02, [(hex 11 01, Pl), (hex 12 01, Pl), (hex 13 01, Pl), (hex 13 02, Pl), (hex 12 03, Pl), (hex 11 02, Pl)]),
  (hex 12 03, [(hex 11 02, Pl), (hex 12 02, Pl), (hex 13 02, Pl), (hex 13 03, Pl), (hex 12 04, Pl), (hex 11 03, Pl)]),
  (hex 12 04, [(hex 11 03, Pl), (hex 12 03, Pl), (hex 13 03, Pl), (hex 13 04, Pl), (hex 12 05, Pl), (hex 11 04, Pl)]),
  (hex 12 05, [(hex 11 04, Pl), (hex 12 04, Pl), (hex 13 04, Pl), (hex 13 05, Pl), (hex 12 06, Pl), (hex 11 05, Pl)]),
  (hex 12 06, [(hex 11 05, Pl), (hex 12 05, Pl), (hex 13 05, Pl), (hex 13 06, Pl), (hex 12 07, Pl), (hex 11 06, Pl)]),
  (hex 12 07, [(hex 11 06, Pl), (hex 12 06, Pl), (hex 13 06, Pl), (hex 13 07, Pl), (hex 12 08, Pl), (hex 11 07, Pl)]),
  (hex 12 08, [(hex 11 07, Pl), (hex 12 07, Pl), (hex 13 07, Pl), (hex 13 08, Pl), (hex 12 09, Pl), (hex 11 08, Pl)]),
  (hex 12 09, [(hex 11 08, Pl), (hex 12 08, Pl), (hex 13 08, Pl), (hex 13 09, Pl), (hex 12 10, Pl), (hex 11 09, Pl)]),
  (hex 12 10, [(hex 11 09, Pl), (hex 12 09, Pl), (hex 13 09, Pl), (hex 13 10, Pl), (hex 12 11, Pl), (hex 11 10, Pl)]),
  (hex 12 11, [(hex 11 10, Pl), (hex 12 10, Pl), (hex 13 10, Pl), (hex 13 11, Pl), (hex 12 12, Pl), (hex 11 11, Pl)]),
  (hex 12 12, [(hex 11 11, Pl), (hex 12 11, Pl), (hex 13 11, Pl), (hex 13 12, Pl), (hex 11 12, Pl)]),
  (hex 13 00, [(hex 12 00, Pl), (hex 14 00, Pl), (hex 14 01, Pl), (hex 13 01, Pl), (hex 12 01, Pl)]),
  (hex 13 01, [(hex 12 01, Pl), (hex 13 00, Pl), (hex 14 01, Pl), (hex 14 02, Pl), (hex 13 02, Pl), (hex 12 02, Pl)]),
  (hex 13 02, [(hex 12 02, Pl), (hex 13 01, Pl), (hex 14 02, Pl), (hex 14 03, Pl), (hex 13 03, Pl), (hex 12 03, Pl)]),
  (hex 13 03, [(hex 12 03, Pl), (hex 13 02, Pl), (hex 14 03, Pl), (hex 14 04, Pl), (hex 13 04, Pl), (hex 12 04, Pl)]),
  (hex 13 04, [(hex 12 04, Pl), (hex 13 03, Pl), (hex 14 04, Pl), (hex 14 05, Pl), (hex 13 05, Pl), (hex 12 05, Pl)]),
  (hex 13 05, [(hex 12 05, Pl), (hex 13 04, Pl), (hex 14 05, Pl), (hex 14 06, Pl), (hex 13 06, Pl), (hex 12 06, Pl)]),
  (hex 13 06, [(hex 12 06, Pl), (hex 13 05, Pl), (hex 14 06, Pl), (hex 14 07, Pl), (hex 13 07, Pl), (hex 12 07, Pl)]),
  (hex 13 07, [(hex 12 07, Pl), (hex 13 06, Pl), (hex 14 07, Pl), (hex 14 08, Pl), (hex 13 08, Pl), (hex 12 08, Pl)]),
  (hex 13 08, [(hex 12 08, Pl), (hex 13 07, Pl), (hex 14 08, Pl), (hex 14 09, Pl), (hex 13 09, Pl), (hex 12 09, Pl)]),
  (hex 13 09, [(hex 12 09, Pl), (hex 13 08, Pl), (hex 14 09, Pl), (hex 14 10, Pl), (hex 13 10, Pl), (hex 12 10, Pl)]),
  (hex 13 10, [(hex 12 10, Pl), (hex 13 09, Pl), (hex 14 10, Pl), (hex 14 11, Pl), (hex 13 11, Pl), (hex 12 11, Pl)]),
  (hex 13 11, [(hex 12 11, Pl), (hex 13 10, Pl), (hex 14 11, Pl), (hex 14 12, Pl), (hex 13 12, Pl), (hex 12 12, Pl)]),
  (hex 13 12, [(hex 12 12, Pl), (hex 13 11, Pl), (hex 14 12, Pl)]),
  (hex 14 00, [(hex 15 00, Pl), (hex 14 01, Pl), (hex 13 00, Pl)]),
  (hex 14 01, [(hex 13 00, Pl), (hex 14 00, Pl), (hex 15 00, Pl), (hex 15 01, Pl), (hex 14 02, Pl), (hex 13 01, Pl)]),
  (hex 14 02, [(hex 13 01, Pl), (hex 14 01, Pl), (hex 15 01, Pl), (hex 15 02, Pl), (hex 14 03, Pl), (hex 13 02, Pl)]),
  (hex 14 03, [(hex 13 02, Pl), (hex 14 02, Pl), (hex 15 02, Pl), (hex 15 03, Pl), (hex 14 04, Pl), (hex 13 03, Pl)]),
  (hex 14 04, [(hex 13 03, Pl), (hex 14 03, Pl), (hex 15 03, Pl), (hex 15 04, Pl), (hex 14 05, Pl), (hex 13 04, Pl)]),
  (hex 14 05, [(hex 13 04, Pl), (hex 14 04, Pl), (hex 15 04, Pl), (hex 15 05, Pl), (hex 14 06, Pl), (hex 13 05, Pl)]),
  (hex 14 06, [(hex 13 05, Pl), (hex 14 05, Pl), (hex 15 05, Pl), (hex 15 06, Pl), (hex 14 07, Pl), (hex 13 06, Pl)]),
  (hex 14 07, [(hex 13 06, Pl), (hex 14 06, Pl), (hex 15 06, Pl), (hex 15 07, Pl), (hex 14 08, Pl), (hex 13 07, Pl)]),
  (hex 14 08, [(hex 13 07, Pl), (hex 14 07, Pl), (hex 15 07, Pl), (hex 15 08, Pl), (hex 14 09, Pl), (hex 13 08, Pl)]),
  (hex 14 09, [(hex 13 08, Pl), (hex 14 08, Pl), (hex 15 08, Pl), (hex 15 09, Pl), (hex 14 10, Pl), (hex 13 09, Pl)]),
  (hex 14 10, [(hex 13 09, Pl), (hex 14 09, Pl), (hex 15 09, Pl), (hex 15 10, Pl), (hex 14 11, Pl), (hex 13 10, Pl)]),
  (hex 14 11, [(hex 13 10, Pl), (hex 14 10, Pl), (hex 15 10, Pl), (hex 15 11, Pl), (hex 14 12, Pl), (hex 13 11, Pl)]),
  (hex 14 12, [(hex 13 11, Pl), (hex 14 11, Pl), (hex 15 11, Pl), (hex 15 12, Pl), (hex 13 12, Pl)]),
  (hex 15 00, [(hex 14 00, Pl), (hex 16 00, Pl), (hex 16 01, Pl), (hex 15 01, Pl), (hex 14 01, Pl)]),
  (hex 15 01, [(hex 14 01, Pl), (hex 15 00, Pl), (hex 16 01, Pl), (hex 16 02, Pl), (hex 15 02, Pl), (hex 14 02, Pl)]),
  (hex 15 02, [(hex 14 02, Pl), (hex 15 01, Pl), (hex 16 02, Pl), (hex 16 03, Pl), (hex 15 03, Pl), (hex 14 03, Pl)]),
  (hex 15 03, [(hex 14 03, Pl), (hex 15 02, Pl), (hex 16 03, Pl), (hex 16 04, Pl), (hex 15 04, Pl), (hex 14 04, Pl)]),
  (hex 15 04, [(hex 14 04, Pl), (hex 15 03, Pl), (hex 16 04, Pl), (hex 16 05, Pl), (hex 15 05, Pl), (hex 14 05, Pl)]),
  (hex 15 05, [(hex 14 05, Pl), (hex 15 04, Pl), (hex 16 05, Pl), (hex 16 06, Pl), (hex 15 06, Pl), (hex 14 06, Pl)]),
  (hex 15 06, [(hex 14 06, Pl), (hex 15 05, Pl), (hex 16 06, Pl), (hex 16 07, Pl), (hex 15 07, Pl), (hex 14 07, Pl)]),
  (hex 15 07, [(hex 14 07, Pl), (hex 15 06, Pl), (hex 16 07, Pl), (hex 16 08, Pl), (hex 15 08, Pl), (hex 14 08, Pl)]),
  (hex 15 08, [(hex 14 08, Pl), (hex 15 07, Pl), (hex 16 08, Pl), (hex 16 09, Pl), (hex 15 09, Pl), (hex 14 09, Pl)]),
  (hex 15 09, [(hex 14 09, Pl), (hex 15 08, Pl), (hex 16 09, Pl), (hex 16 10, Pl), (hex 15 10, Pl), (hex 14 10, Pl)]),
  (hex 15 10, [(hex 14 10, Pl), (hex 15 09, Pl), (hex 16 10, Pl), (hex 16 11, Pl), (hex 15 11, Pl), (hex 14 11, Pl)]),
  (hex 15 11, [(hex 14 11, Pl), (hex 15 10, Pl), (hex 16 11, Pl), (hex 16 12, Pl), (hex 15 12, Pl), (hex 14 12, Pl)]),
  (hex 15 12, [(hex 14 12, Pl), (hex 15 11, Pl), (hex 16 12, Pl)]),
  (hex 16 00, [(hex 17 00, Pl), (hex 16 01, Pl), (hex 15 00, Pl)]),
  (hex 16 01, [(hex 15 00, Pl), (hex 16 00, Pl), (hex 17 00, Pl), (hex 17 01, Pl), (hex 16 02, Pl), (hex 15 01, Pl)]),
  (hex 16 02, [(hex 15 01, Pl), (hex 16 01, Pl), (hex 17 01, Pl), (hex 17 02, Pl), (hex 16 03, Pl), (hex 15 02, Pl)]),
  (hex 16 03, [(hex 15 02, Pl), (hex 16 02, Pl), (hex 17 02, Pl), (hex 17 03, Pl), (hex 16 04, Pl), (hex 15 03, Pl)]),
  (hex 16 04, [(hex 15 03, Pl), (hex 16 03, Pl), (hex 17 03, Pl), (hex 17 04, Pl), (hex 16 05, Pl), (hex 15 04, Pl)]),
  (hex 16 05, [(hex 15 04, Pl), (hex 16 04, Pl), (hex 17 04, Pl), (hex 17 05, Pl), (hex 16 06, Pl), (hex 15 05, Pl)]),
  (hex 16 06, [(hex 15 05, Pl), (hex 16 05, Pl), (hex 17 05, Pl), (hex 17 06, Pl), (hex 16 07, Pl), (hex 15 06, Pl)]),
  (hex 16 07, [(hex 15 06, Pl), (hex 16 06, Pl), (hex 17 06, Pl), (hex 17 07, Pl), (hex 16 08, Pl), (hex 15 07, Pl)]),
  (hex 16 08, [(hex 15 07, Pl), (hex 16 07, Pl), (hex 17 07, Pl), (hex 17 08, Pl), (hex 16 09, Pl), (hex 15 08, Pl)]),
  (hex 16 09, [(hex 15 08, Pl), (hex 16 08, Pl), (hex 17 08, Pl), (hex 17 09, Pl), (hex 16 10, Pl), (hex 15 09, Pl)]),
  (hex 16 10, [(hex 15 09, Pl), (hex 16 09, Pl), (hex 17 09, Pl), (hex 17 10, Pl), (hex 16 11, Pl), (hex 15 10, Pl)]),
  (hex 16 11, [(hex 15 10, Pl), (hex 16 10, Pl), (hex 17 10, Pl), (hex 17 11, Pl), (hex 16 12, Pl), (hex 15 11, Pl)]),
  (hex 16 12, [(hex 15 11, Pl), (hex 16 11, Pl), (hex 17 11, Pl), (hex 17 12, Pl), (hex 15 12, Pl)]),
  (hex 17 00, [(hex 16 00, Pl), (hex 18 00, Pl), (hex 18 01, Pl), (hex 17 01, Pl), (hex 16 01, Pl)]),
  (hex 17 01, [(hex 16 01, Pl), (hex 17 00, Pl), (hex 18 01, Pl), (hex 18 02, Pl), (hex 17 02, Pl), (hex 16 02, Pl)]),
  (hex 17 02, [(hex 16 02, Pl), (hex 17 01, Pl), (hex 18 02, Pl), (hex 18 03, Pl), (hex 17 03, Pl), (hex 16 03, Pl)]),
  (hex 17 03, [(hex 16 03, Pl), (hex 17 02, Pl), (hex 18 03, Pl), (hex 18 04, Pl), (hex 17 04, Pl), (hex 16 04, Pl)]),
  (hex 17 04, [(hex 16 04, Pl), (hex 17 03, Pl), (hex 18 04, Pl), (hex 18 05, Pl), (hex 17 05, Pl), (hex 16 05, Pl)]),
  (hex 17 05, [(hex 16 05, Pl), (hex 17 04, Pl), (hex 18 05, Pl), (hex 18 06, Pl), (hex 17 06, Pl), (hex 16 06, Pl)]),
  (hex 17 06, [(hex 16 06, Pl), (hex 17 05, Pl), (hex 18 06, Pl), (hex 18 07, Pl), (hex 17 07, Pl), (hex 16 07, Pl)]),
  (hex 17 07, [(hex 16 07, Pl), (hex 17 06, Pl), (hex 18 07, Pl), (hex 18 08, Pl), (hex 17 08, Pl), (hex 16 08, Pl)]),
  (hex 17 08, [(hex 16 08, Pl), (hex 17 07, Pl), (hex 18 08, Pl), (hex 18 09, Pl), (hex 17 09, Pl), (hex 16 09, Pl)]),
  (hex 17 09, [(hex 16 09, Pl), (hex 17 08, Pl), (hex 18 09, Pl), (hex 18 10, Pl), (hex 17 10, Pl), (hex 16 10, Pl)]),
  (hex 17 10, [(hex 16 10, Pl), (hex 17 09, Pl), (hex 18 10, Pl), (hex 18 11, Pl), (hex 17 11, Pl), (hex 16 11, Pl)]),
  (hex 17 11, [(hex 16 11, Pl), (hex 17 10, Pl), (hex 18 11, Pl), (hex 18 12, Pl), (hex 17 12, Pl), (hex 16 12, Pl)]),
  (hex 17 12, [(hex 16 12, Pl), (hex 17 11, Pl), (hex 18 12, Pl)]),
  (hex 18 00, [(hex 19 00, Pl), (hex 18 01, Pl), (hex 17 00, Pl)]),
  (hex 18 01, [(hex 17 00, Pl), (hex 18 00, Pl), (hex 19 00, Pl), (hex 19 01, Pl), (hex 18 02, Pl), (hex 17 01, Pl)]),
  (hex 18 02, [(hex 17 01, Pl), (hex 18 01, Pl), (hex 19 01, Pl), (hex 19 02, Pl), (hex 18 03, Pl), (hex 17 02, Pl)]),
  (hex 18 03, [(hex 17 02, Pl), (hex 18 02, Pl), (hex 19 02, Pl), (hex 19 03, Pl), (hex 18 04, Pl), (hex 17 03, Pl)]),
  (hex 18 04, [(hex 17 03, Pl), (hex 18 03, Pl), (hex 19 03, Pl), (hex 19 04, Pl), (hex 18 05, Pl), (hex 17 04, Pl)]),
  (hex 18 05, [(hex 17 04, Pl), (hex 18 04, Pl), (hex 19 04, Pl), (hex 19 05, Pl), (hex 18 06, Pl), (hex 17 05, Pl)]),
  (hex 18 06, [(hex 17 05, Pl), (hex 18 05, Pl), (hex 19 05, Pl), (hex 19 06, Pl), (hex 18 07, Pl), (hex 17 06, Pl)]),
  (hex 18 07, [(hex 17 06, Pl), (hex 18 06, Pl), (hex 19 06, Pl), (hex 19 07, Pl), (hex 18 08, Pl), (hex 17 07, Pl)]),
  (hex 18 08, [(hex 17 07, Pl), (hex 18 07, Pl), (hex 19 07, Pl), (hex 19 08, Pl), (hex 18 09, Pl), (hex 17 08, Pl)]),
  (hex 18 09, [(hex 17 08, Pl), (hex 18 08, Pl), (hex 19 08, Pl), (hex 19 09, Pl), (hex 18 10, Pl), (hex 17 09, Pl)]),
  (hex 18 10, [(hex 17 09, Pl), (hex 18 09, Pl), (hex 19 09, Pl), (hex 19 10, Pl), (hex 18 11, Pl), (hex 17 10, Pl)]),
  (hex 18 11, [(hex 17 10, Pl), (hex 18 10, Pl), (hex 19 10, Pl), (hex 19 11, Pl), (hex 18 12, Pl), (hex 17 11, Pl)]),
  (hex 18 12, [(hex 17 11, Pl), (hex 18 11, Pl), (hex 19 11, Pl), (hex 19 12, Pl), (hex 17 12, Pl)]),
  (hex 19 00, [(hex 18 00, Pl), (hex 20 00, Pl), (hex 20 01, Pl), (hex 19 01, Pl), (hex 18 01, Pl)]),
  (hex 19 01, [(hex 18 01, Pl), (hex 19 00, Pl), (hex 20 01, Pl), (hex 20 02, Pl), (hex 19 02, Pl), (hex 18 02, Pl)]),
  (hex 19 02, [(hex 18 02, Pl), (hex 19 01, Pl), (hex 20 02, Pl), (hex 20 03, Pl), (hex 19 03, Pl), (hex 18 03, Pl)]),
  (hex 19 03, [(hex 18 03, Pl), (hex 19 02, Pl), (hex 20 03, Pl), (hex 20 04, Pl), (hex 19 04, Pl), (hex 18 04, Pl)]),
  (hex 19 04, [(hex 18 04, Pl), (hex 19 03, Pl), (hex 20 04, Pl), (hex 20 05, Pl), (hex 19 05, Pl), (hex 18 05, Pl)]),
  (hex 19 05, [(hex 18 05, Pl), (hex 19 04, Pl), (hex 20 05, Pl), (hex 20 06, Pl), (hex 19 06, Pl), (hex 18 06, Pl)]),
  (hex 19 06, [(hex 18 06, Pl), (hex 19 05, Pl), (hex 20 06, Pl), (hex 20 07, Pl), (hex 19 07, Pl), (hex 18 07, Pl)]),
  (hex 19 07, [(hex 18 07, Pl), (hex 19 06, Pl), (hex 20 07, Pl), (hex 20 08, Pl), (hex 19 08, Pl), (hex 18 08, Pl)]),
  (hex 19 08, [(hex 18 08, Pl), (hex 19 07, Pl), (hex 20 08, Pl), (hex 20 09, Pl), (hex 19 09, Pl), (hex 18 09, Pl)]),
  (hex 19 09, [(hex 18 09, Pl), (hex 19 08, Pl), (hex 20 09, Pl), (hex 20 10, Pl), (hex 19 10, Pl), (hex 18 10, Pl)]),
  (hex 19 10, [(hex 18 10, Pl), (hex 19 09, Pl), (hex 20 10, Pl), (hex 20 11, Pl), (hex 19 11, Pl), (hex 18 11, Pl)]),
  (hex 19 11, [(hex 18 11, Pl), (hex 19 10, Pl), (hex 20 11, Pl), (hex 20 12, Pl), (hex 19 12, Pl), (hex 18 12, Pl)]),
  (hex 19 12, [(hex 18 12, Pl), (hex 19 11, Pl), (hex 20 12, Pl)]),
  (hex 20 00, [(hex 21 00, Pl), (hex 20 01, Pl), (hex 19 00, Pl)]),
  (hex 20 01, [(hex 19 00, Pl), (hex 20 00, Pl), (hex 21 00, Pl), (hex 21 01, Pl), (hex 20 02, Pl), (hex 19 01, Pl)]),
  (hex 20 02, [(hex 19 01, Pl), (hex 20 01, Pl), (hex 21 01, Pl), (hex 21 02, Pl), (hex 20 03, Pl), (hex 19 02, Pl)]),
  (hex 20 03, [(hex 19 02, Pl), (hex 20 02, Pl), (hex 21 02, Pl), (hex 21 03, Pl), (hex 20 04, Pl), (hex 19 03, Pl)]),
  (hex 20 04, [(hex 19 03, Pl), (hex 20 03, Pl), (hex 21 03, Pl), (hex 21 04, Pl), (hex 20 05, Pl), (hex 19 04, Pl)]),
  (hex 20 05, [(hex 19 04, Pl), (hex 20 04, Pl), (hex 21 04, Pl), (hex 21 05, Pl), (hex 20 06, Pl), (hex 19 05, Pl)]),
  (hex 20 06, [(hex 19 05, Pl), (hex 20 05, Pl), (hex 21 05, Pl), (hex 21 06, Pl), (hex 20 07, Pl), (hex 19 06, Pl)]),
  (hex 20 07, [(hex 19 06, Pl), (hex 20 06, Pl), (hex 21 06, Pl), (hex 21 07, Pl), (hex 20 08, Pl), (hex 19 07, Pl)]),
  (hex 20 08, [(hex 19 07, Pl), (hex 20 07, Pl), (hex 21 07, Pl), (hex 21 08, Pl), (hex 20 09, Pl), (hex 19 08, Pl)]),
  (hex 20 09, [(hex 19 08, Pl), (hex 20 08, Pl), (hex 21 08, Pl), (hex 21 09, Pl), (hex 20 10, Pl), (hex 19 09, Pl)]),
  (hex 20 10, [(hex 19 09, Pl), (hex 20 09, Pl), (hex 21 09, Pl), (hex 21 10, Pl), (hex 20 11, Pl), (hex 19 10, Pl)]),
  (hex 20 11, [(hex 19 10, Pl), (hex 20 10, Pl), (hex 21 10, Pl), (hex 21 11, Pl), (hex 20 12, Pl), (hex 19 11, Pl)]),
  (hex 20 12, [(hex 19 11, Pl), (hex 20 11, Pl), (hex 21 11, Pl), (hex 21 12, Pl), (hex 19 12, Pl)]),
  (hex 21 00, [(hex 20 00, Pl), (hex 22 00, Pl), (hex 22 01, Pl), (hex 21 01, Pl), (hex 20 01, Pl)]),
  (hex 21 01, [(hex 20 01, Pl), (hex 21 00, Pl), (hex 22 01, Pl), (hex 22 02, Pl), (hex 21 02, Pl), (hex 20 02, Pl)]),
  (hex 21 02, [(hex 20 02, Pl), (hex 21 01, Pl), (hex 22 02, Pl), (hex 22 03, Pl), (hex 21 03, Pl), (hex 20 03, Pl)]),
  (hex 21 03, [(hex 20 03, Pl), (hex 21 02, Pl), (hex 22 03, Pl), (hex 22 04, Pl), (hex 21 04, Pl), (hex 20 04, Pl)]),
  (hex 21 04, [(hex 20 04, Pl), (hex 21 03, Pl), (hex 22 04, Pl), (hex 22 05, Pl), (hex 21 05, Pl), (hex 20 05, Pl)]),
  (hex 21 05, [(hex 20 05, Pl), (hex 21 04, Pl), (hex 22 05, Pl), (hex 22 06, Pl), (hex 21 06, Pl), (hex 20 06, Pl)]),
  (hex 21 06, [(hex 20 06, Pl), (hex 21 05, Pl), (hex 22 06, Pl), (hex 22 07, Pl), (hex 21 07, Pl), (hex 20 07, Pl)]),
  (hex 21 07, [(hex 20 07, Pl), (hex 21 06, Pl), (hex 22 07, Pl), (hex 22 08, Pl), (hex 21 08, Pl), (hex 20 08, Pl)]),
  (hex 21 08, [(hex 20 08, Pl), (hex 21 07, Pl), (hex 22 08, Pl), (hex 22 09, Pl), (hex 21 09, Pl), (hex 20 09, Pl)]),
  (hex 21 09, [(hex 20 09, Pl), (hex 21 08, Pl), (hex 22 09, Pl), (hex 22 10, Pl), (hex 21 10, Pl), (hex 20 10, Pl)]),
  (hex 21 10, [(hex 20 10, Pl), (hex 21 09, Pl), (hex 22 10, Pl), (hex 22 11, Pl), (hex 21 11, Pl), (hex 20 11, Pl)]),
  (hex 21 11, [(hex 20 11, Pl), (hex 21 10, Pl), (hex 22 11, Pl), (hex 22 12, Pl), (hex 21 12, Pl), (hex 20 12, Pl)]),
  (hex 21 12, [(hex 20 12, Pl), (hex 21 11, Pl), (hex 22 12, Pl)]),
  (hex 22 00, [(hex 22 01, Pl), (hex 21 00, Pl)]),
  (hex 22 01, [(hex 21 00, Pl), (hex 22 00, Pl), (hex 22 02, Pl), (hex 21 01, Pl)]),
  (hex 22 02, [(hex 21 01, Pl), (hex 22 01, Pl), (hex 22 03, Pl), (hex 21 02, Pl)]),
  (hex 22 03, [(hex 21 02, Pl), (hex 22 02, Pl), (hex 22 04, Pl), (hex 21 03, Pl)]),
  (hex 22 04, [(hex 21 03, Pl), (hex 22 03, Pl), (hex 22 05, Pl), (hex 21 04, Pl)]),
  (hex 22 05, [(hex 21 04, Pl), (hex 22 04, Pl), (hex 22 06, Pl), (hex 21 05, Pl)]),
  (hex 22 06, [(hex 21 05, Pl), (hex 22 05, Pl), (hex 22 07, Pl), (hex 21 06, Pl)]),
  (hex 22 07, [(hex 21 06, Pl), (hex 22 06, Pl), (hex 22 08, Pl), (hex 21 07, Pl)]),
  (hex 22 08, [(hex 21 07, Pl), (hex 22 07, Pl), (hex 22 09, Pl), (hex 21 08, Pl)]),
  (hex 22 09, [(hex 21 08, Pl), (hex 22 08, Pl), (hex 22 10, Pl), (hex 21 09, Pl)]),
  (hex 22 10, [(hex 21 09, Pl), (hex 22 09, Pl), (hex 22 11, Pl), (hex 21 10, Pl)]),
  (hex 22 11, [(hex 21 10, Pl), (hex 22 10, Pl), (hex 22 12, Pl), (hex 21 11, Pl)]),
  (hex 22 12, [(hex 21 11, Pl), (hex 22 11, Pl), (hex 21 12, Pl)])
      ]

public export
PartialGameMap : Map
PartialGameMap = MkMap positions []
  where
    positions : List (Pos, Terrain)
    positions = [(hex 0 0, Clear),
                 (hex 0 1, Clear),
                 (hex 0 2, Clear),
                 (hex 0 3, SupplySource Axis $ Clear),
                 (hex 0 4, Clear),
                 (hex 1 0, Clear),
                 (hex 1 1, Clear),
                 (hex 1 2, Clear),
                 (hex 1 3, Clear),
                 (hex 1 4, Clear),
                 (hex 2 0, Clear),
                 (hex 2 1, Clear),
                 (hex 2 2, Clear),
                 (hex 2 3, Clear),
                 (hex 2 4, Clear),
                 (hex 3 0, Clear),
                 (hex 3 1, Clear),
                 (hex 3 2, Clear),
                 (hex 3 3, Clear),
                 (hex 3 4, Clear),
                 (hex 4 0, Clear),
                 (hex 4 1, Clear),
                 (hex 4 2, Clear),
                 (hex 4 3, Clear),
                 (hex 4 4, Clear)]

public export
FullGameMap : Map
FullGameMap = MkMap positions links
  where
    mkPosition : (c : Nat) -> (r : Nat) -> Maybe ( Pos, Terrain)
    mkPosition c r with (natToFin c 23, natToFin r 13, isLTE c 22, isLTE r 12)
      mkPosition c r | (Just col, Just row, Yes cprf, Yes rprf) =
           let terrain = index col $ index row terrains
           in Just (hex col row, terrain)
      mkPosition c r | (_, _, _, _) = Nothing

    positions : List (Pos, Terrain)
    positions = catMaybes [ mkPosition c r | c <- [ 0 .. 22 ], r <- [ 0 .. 12 ]]
