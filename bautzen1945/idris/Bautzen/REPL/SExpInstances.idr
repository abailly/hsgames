module Bautzen.REPL.SExpInstances

import Bautzen.GameUnit
import Bautzen.Game.Core
import Bautzen.Pos
import Bautzen.Terrain
import Bautzen.SExp

%access export
%default total

ToSExp Side where
  toSExp Axis = SSym "Axis"
  toSExp Allies = SSym "Allies"

ToSExp Nation where
  toSExp German = SSym "German"
  toSExp Russian = SSym "Russian"
  toSExp Polish = SSym "Polish"

ToSExp UnitType where
  toSExp Armored = SSym "Armored"
  toSExp HeavyArmored = SSym "HeavyArmored"
  toSExp MechInfantry = SSym "MechInfantry"
  toSExp Infantry = SSym "Infantry"
  toSExp HeavyEngineer = SSym "HeavyEngineer"
  toSExp Artillery = SSym "Artillery"
  toSExp AntiTank = SSym "AntiTank"
  toSExp HQ = SSym "HQ"
  toSExp SupplyColumn = SSym "SupplyColumn"

ToSExp UnitSize where
  toSExp Regiment = SSym "Regiment"
  toSExp Brigade = SSym "Brigade"
  toSExp Division = SSym "Division"
  toSExp Corps = SSym "Corps"
  toSExp Army = SSym "Army"

ToSExp StdFactors where
  toSExp (MkStdFactors attack defense) =
    SList [ toSExp attack
          , toSExp defense
          ]

ToSExp Arty where
  toSExp (MkArty support distance) =
    SList [ toSExp support
          , toSExp distance
          ]

ToSExp Pak where
  toSExp (MkPak antitank) = toSExp antitank

ToSExp GameUnit where
  toSExp (MkGameUnit nation unitType name parent size move currentMP hit combat) =
    SList [ SSym ":unit"
          , toSExp nation
          , toSExp unitType
          , toSExp name
          , toSExp parent
          , toSExp size
          , toSExp move
          , toSExp currentMP
          , toSExp hit
          , case unitType of
              Armored       => toSExp combat
              HeavyArmored  => toSExp combat
              MechInfantry  => toSExp combat
              Infantry      => toSExp combat
              HeavyEngineer => toSExp combat
              Artillery     => toSExp combat
              AntiTank      => toSExp combat
              HQ            => toSExp combat
              SupplyColumn  => toSExp combat
          ]

ToSExp Pos where
  toSExp (Hex col row) =
    SList [ toSExp col
          , toSExp row
          ]

ToSExp Cost where
  toSExp = toSExp . toNat

ToSExp GameError where
  toSExp (NoSuchUnits unitNames) = SList [ SSym ":error", SSym "NoSuchUnit", toSExp unitNames ]
  toSExp (NotYourTurn side) = SList [ SSym ":error", SSym "NoSuchUnit", toSExp side ]
  toSExp (EnemyInHex unit hex) = SList [ SSym ":error", SSym "EnemyInHex", toSExp unit, toSExp hex ]
  toSExp (MoveFromZocToZoc unit to) = SList [ SSym ":error", SSym "MoveFromZocToZoc", toSExp unit, toSExp to ]
  toSExp (ForbiddenTerrain from to) = SList [ SSym ":error", SSym "ForbiddenTerrain", toSExp from, toSExp to ]
  toSExp (InvalidMove from to) = SList [ SSym ":error", SSym "InvalidMove", toSExp from, toSExp to ]
  toSExp (NotEnoughMPs unit from to mp) = SList [ SSym ":error", SSym "NotEnoughMPs", toSExp unit, toSExp from, toSExp to, toSExp mp ]
  toSExp (NotAdjacentTo units to) = SList [ SSym ":error", SSym "NotAdjacentTo", toSExp units , toSExp to ]

ToSExp Event where
  toSExp (Moved unit from to cost) =
      SList [ SSym ":moved"
            , SSym ":unit", toSExp unit
            , SSym ":from", toSExp from
            , SSym ":to", toSExp to
            , SSym ":cost", toSExp cost
            ]

splice : SExp -> SExp -> SExp
splice first (SList xs) = SList $ first :: xs
splice first s  = SList [ first, s ]

ToSExp Terrain where
  toSExp Clear = SSym "Clear"
  toSExp Wood = SSym "Wood"
  toSExp Rough = SSym "Rough"
  toSExp RoughWood = SSym "RoughWood"
  toSExp (Hill base) = splice (SSym "Hill") (toSExp base)
  toSExp (Village base) = splice (SSym "Village") (toSExp base)
  toSExp Town = SSym "Town"
  toSExp (SupplySource side base) = splice (SList [ SSym "Supply", toSExp side] ) (toSExp base)

ToSExp Connection where
  toSExp Plain = SSym "Plain"
  toSExp (Road base) = splice (SSym "Road") (toSExp base)
  toSExp (River base) = splice (SSym "River") (toSExp base)
  toSExp Lake = SSym "Lake"

ToSExp Map where
  toSExp (MkMap hexes edges) = SList [ SList [ SSym ":hexes", toSExp (tabulate hexes) ]
                                     , SList [ SSym ":edges", toSExp edges ]
                                     ]
