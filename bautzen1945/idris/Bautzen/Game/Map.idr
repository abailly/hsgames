module Bautzen.Game.Map

import Bautzen.Terrain
import Bautzen.GameUnit
import Bautzen.Pos

export
GameMap : Map
GameMap = MkMap positions []
  where
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
