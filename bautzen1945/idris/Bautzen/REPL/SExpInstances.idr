module Bautzen.REPL.SExpInstances

import Bautzen.Combats
import Bautzen.GameUnit
import Bautzen.Game.Core
import Bautzen.Pos as P
import Bautzen.Terrain
import Bautzen.SExp

import Data.Fin
import Data.Vect


export
ToSExp Side where
  toSExp Axis = SSym "Axis"
  toSExp Allies = SSym "Allies"

export
ToSExp Nation where
  toSExp German = SSym "German"
  toSExp Russian = SSym "Russian"
  toSExp Polish = SSym "Polish"

export
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

export
ToSExp UnitSize where
  toSExp Regiment = SSym "Regiment"
  toSExp Brigade = SSym "Brigade"
  toSExp Division = SSym "Division"
  toSExp Corps = SSym "Corps"
  toSExp Army = SSym "Army"

export
ToSExp StdFactors where
  toSExp (MkStdFactors attack defense) =
    SList [ toSExp attack
          , toSExp defense
          ]

export
ToSExp Arty where
  toSExp (MkArty support distance) =
    SList [ toSExp support
          , toSExp distance
          ]

export
ToSExp Pak where
  toSExp (MkPak antitank) = toSExp antitank

export
ToSExp GameUnit where
  toSExp (MkGameUnit nation unitType name parent size move currentMP steps hits combat) =
    SList [ SSym ":unit"
          , toSExp nation
          , toSExp unitType
          , toSExp name
          , toSExp parent
          , toSExp size
          , toSExp move
          , toSExp currentMP
          , toSExp steps
          , toSExp hits
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

public export
ToSExp (P.Loc c r) where
  toSExp (P.Hex col row) =
    SList [ toSExp col
          , toSExp row
          ]

export
ToSExp Pos where
  toSExp (MkPos p) = toSExp p

export
ToSExp Cost where
  toSExp = toSExp . toNat

export
ToSExp GameError where
  toSExp (NoSuchUnits unitNames) = SList [ SSym ":error", SSym "NoSuchUnit", toSExp unitNames ]
  toSExp (NotYourTurn side) = SList [ SSym ":error", SSym "NotYourTurn", toSExp side ]
  toSExp (EnemyInHex unit hex) = SList [ SSym ":error", SSym "EnemyInHex", toSExp unit, toSExp hex ]
  toSExp (MoveFromZocToZoc unit to) = SList [ SSym ":error", SSym "MoveFromZocToZoc", toSExp unit, toSExp to ]
  toSExp (ForbiddenTerrain from to) = SList [ SSym ":error", SSym "ForbiddenTerrain", toSExp from, toSExp to ]
  toSExp (InvalidMove from to) = SList [ SSym ":error", SSym "InvalidMove", toSExp from, toSExp to ]
  toSExp (NotEnoughMPs unit from to mp) = SList [ SSym ":error", SSym "NotEnoughMPs", toSExp unit, toSExp from, toSExp to, toSExp mp ]
  toSExp (NotAdjacentTo units to) = SList [ SSym ":error", SSym "NotAdjacentTo", toSExp units , toSExp to ]
  toSExp (NothingToAttack target) = SList [ SSym ":error", SSym "NothingToAttack", toSExp target ]
  toSExp (AttackingOwnUnits units target) = SList [ SSym ":error", SSym "AttackingOwnUnits", toSExp units , toSExp target ]
  toSExp (NotInSupportRange units) = SList [ SSym ":error", SSym "NotInSupportRange", toSExp units ]
  toSExp (NotSupportingUnits units) = SList [ SSym ":error", SSym "NotSupportingUnits", toSExp units ]
  toSExp (NotInChainOfCommand units) = SList [ SSym ":error", SSym "NotInChainOfCommand", toSExp units ]
  toSExp (NoSupplyColumnThere hex) = SList [ SSym ":error", SSym "NoSupplyColumnThere", toSExp hex ]
  toSExp (NoStepsToLose side) = SList [ SSym ":error", SSym "NoStepsToLose", toSExp side ]
  toSExp (CombatInProgress side) = SList [ SSym ":error", SSym "CombatInProgress", toSExp side ]
  toSExp GameHasEnded = SList [ SSym ":error", SSym "GameHasEnded" ]

export
ToSExp Losses where
  toSExp (attackerLoss /> defenderLoss) = SList [ toSExp attackerLoss, toSExp defenderLoss ]

export
ToSExp EngagedUnits where
  toSExp (MkEngagedUnits base tacticalSupport strategicSupport) =
    SList [ SSym ":engaged"
          , SSym ":base", toSExp base
          , SSym ":tactical-support", toSExp tacticalSupport
          , SSym ":strategic-support", toSExp strategicSupport
          ]

export
ToSExp CombatState where
  toSExp (MkCombatState hex attackers defenders losses) =
    SList [ SSym ":combat-state"
          , SSym ":combat-hex", toSExp hex
          , SSym ":attackers", toSExp attackers
          , SSym ":defenders", toSExp defenders
          , SSym ":losses", toSExp losses
          ]

export
ToSExp CombatPhase where
  toSExp NoCombat = SSym "NoCombat"
  toSExp (AssignTacticalSupport side combat) = SList [ SSym "AssignTacticalSupport",  toSExp side, toSExp combat ]
  toSExp (AssignStrategicSupport side combat) = SList [ SSym "AssignStrategicSupport",  toSExp side, toSExp combat ]
  toSExp (ApplyLosses side combat) = SList [ SSym "ApplyLosses",  toSExp side, toSExp combat ]
  toSExp (Resolve combat) = SList [ SSym "Resolve", toSExp combat ]

export
ToSExp GameSegment where
  toSExp Supply = SSym "Supply"
  toSExp Move = SSym "Move"
  toSExp (Combat phase) = SList [ SSym "Combat", toSExp phase ]
  toSExp GameEnd = SSym "GameEnd"

export
ToSExp Event where
  toSExp (Moved unit from to cost) =
      SList [ SSym ":moved"
            , SSym ":unit", toSExp unit
            , SSym ":from", toSExp from
            , SSym ":to", toSExp to
            , SSym ":cost", toSExp cost
            ]
  toSExp (CombatEngaged atk def target) =
      SList [ SSym ":combat-engaged"
            , SSym ":attackers", toSExp atk
            , SSym ":defenders", toSExp def
            , SSym ":target", toSExp target
            ]
  toSExp (TacticalSupportProvided side units) =
      SList [ SSym ":tactical-support-provided"
            , SSym ":supported-side", toSExp side
            , SSym ":supporting-units", toSExp units
            ]
  toSExp (SupplyColumnUsed side hex) =
      SList [ SSym ":supply-column-used"
            , SSym ":supported-side", toSExp side
            , SSym ":position", toSExp hex
            ]
  toSExp (CombatResolved state losses) =
      SList [ SSym ":combat-resolved"
            , SSym ":state", toSExp state
            , SSym ":losses", toSExp losses
            ]
  toSExp (StepLost side unit remain) =
      SList [ SSym ":step-lost"
            , SSym ":side", toSExp side
            , SSym ":unit", toSExp unit
            , SSym ":remaining-losses", toSExp remain
            ]
  toSExp (SegmentChanged from to) =
      SList [ SSym ":segment-changed"
            , SSym ":from", toSExp from
            , SSym ":to", toSExp to
            ]
  toSExp (TurnEnded n) =
      SList [ SSym ":turn-ended"
            , SSym ":new-turn", toSExp n
            ]
  toSExp AxisTurnDone =
      SList [ SSym ":axis-turn-done" ]
  toSExp GameEnded =
      SList [ SSym ":game-ended" ]

export
splice : SExp -> SExp -> SExp
splice first (SList xs) = SList $ first :: xs
splice first s  = SList [ first, s ]

export
ToSExp Terrain where
  toSExp Clear = SSym "Clear"
  toSExp Wood = SSym "Wood"
  toSExp Rough = SSym "Rough"
  toSExp RoughWood = SSym "RoughWood"
  toSExp (Hill base) = splice (SSym "Hill") (toSExp base)
  toSExp (Village base) = splice (SSym "Village") (toSExp base)
  toSExp Town = SSym "Town"
  toSExp (SupplySource side base) = splice (SList [ SSym "Supply", toSExp side] ) (toSExp base)

export
ToSExp Connection where
  toSExp Plain = SSym "Plain"
  toSExp (Road base) = splice (SSym "Road") (toSExp base)
  toSExp (River base) = splice (SSym "River") (toSExp base)
  toSExp Lake = SSym "Lake"

export
ToSExp Map where
  toSExp (MkMap hexes edges) = SList [ SList [ SSym ":hexes", toSExp (tabulate hexes) ]
                                     , SList [ SSym ":edges", toSExp edges ]
                                     ]
export
ToSExp QueryError where
  toSExp (NoSupplyPathFor unitName pos) = SList [ SSym "NoSupplyPathFor", SStr unitName, toSExp pos ]
  toSExp (UnitDoesNotExist unitName) = SList [ SSym "UnitDoesNotExist", SStr unitName ]
