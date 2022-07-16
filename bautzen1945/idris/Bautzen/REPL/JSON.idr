module Bautzen.REPL.JSON

import Bautzen.Combats
import Bautzen.GameUnit
import Bautzen.Game.Core
import Bautzen.Pos as P
import Bautzen.Terrain
import Language.JSON
import Language.JSON.Data

import Data.Fin
import Data.Vect
import Control.WellFounded


export
Cast Side JSON where
  cast Axis = JString "Axis"
  cast Allies = JString "Allies"

export
Cast Nation JSON where
  cast German = JString "German"
  cast Russian = JString "Russian"
  cast Polish = JString "Polish"

export
Cast UnitType JSON where
  cast Armored = JString "Armored"
  cast HeavyArmored = JString "HeavyArmored"
  cast MechInfantry = JString "MechInfantry"
  cast Infantry = JString "Infantry"
  cast HeavyEngineer = JString "HeavyEngineer"
  cast Artillery = JString "Artillery"
  cast AntiTank = JString "AntiTank"
  cast HQ = JString "HQ"
  cast SupplyColumn = JString "SupplyColumn"

export
Cast UnitSize JSON where
  cast Regiment = JString "Regiment"
  cast Brigade = JString "Brigade"
  cast Division = JString "Division"
  cast Corps = JString "Corps"
  cast Army = JString "Army"

export
Cast Nat JSON where
  cast = cast . cast {to = Double} . natToInteger

export
Cast StdFactors JSON where
  cast (MkStdFactors attack defense) =
    JArray [ cast attack
          , cast defense
          ]

export
Cast Arty JSON where
  cast (MkArty support distance) =
    JArray [ cast support
          , cast distance
          ]

export
Cast Pak JSON where
  cast (MkPak antitank) = cast antitank

export
Cast (Fin n) JSON where
  cast = cast . cast { to = Double} . finToInteger

export
Cast a JSON => Cast (Maybe a) JSON where
  cast Nothing = JNull
  cast (Just a) = cast a

export
Cast a JSON => Cast (Vect n a) JSON where
  cast = cast . toList

export
Cast a JSON => Cast b JSON => Cast (Either a b) JSON where
  cast (Right r) = cast r
  cast (Left l) = cast l

export
Cast a JSON => Cast b JSON => Cast c JSON => Cast (a, b, c) JSON where
  cast (a,b,c) = JArray [ cast a, cast b, cast c ]

export
Cast GameUnit JSON where
  cast (MkGameUnit nation unitType name parent size move currentMP steps hits combat) =
    JArray [ JString ":unit"
          , cast nation
          , cast unitType
          , cast name
          , cast parent
          , cast size
          , cast move
          , cast currentMP
          , cast steps
          , cast hits
          , case unitType of
              Armored       => cast combat
              HeavyArmored  => cast combat
              MechInfantry  => cast combat
              Infantry      => cast combat
              HeavyEngineer => cast combat
              Artillery     => cast combat
              AntiTank      => cast combat
              HQ            => cast combat
              SupplyColumn  => cast combat
          ]

public export
Cast (P.Loc c r) JSON where
  cast (P.Hex col row) =
    JArray [ cast col
          , cast row
          ]

export
Cast Pos JSON where
  cast (MkPos p) = cast p

export
Cast Cost JSON where
  cast = cast . toNat

export
Cast GameError JSON where
  cast (NoSuchUnits unitNames) = JArray [ JString ":error", JString "NoSuchUnit", cast unitNames ]
  cast (NotYourTurn side) = JArray [ JString ":error", JString "NotYourTurn", cast side ]
  cast (EnemyInHex unit hex) = JArray [ JString ":error", JString "EnemyInHex", cast unit, cast hex ]
  cast (MoveFromZocToZoc unit to) = JArray [ JString ":error", JString "MoveFromZocToZoc", cast unit, cast to ]
  cast (ForbiddenTerrain from to) = JArray [ JString ":error", JString "ForbiddenTerrain", cast from, cast to ]
  cast (InvalidMove from to) = JArray [ JString ":error", JString "InvalidMove", cast from, cast to ]
  cast (NotEnoughMPs unit from to mp) = JArray [ JString ":error", JString "NotEnoughMPs", cast unit, cast from, cast to, cast mp ]
  cast (NotAdjacentTo units to) = JArray [ JString ":error", JString "NotAdjacentTo", cast units , cast to ]
  cast (NothingToAttack target) = JArray [ JString ":error", JString "NothingToAttack", cast target ]
  cast (AttackingOwnUnits units target) = JArray [ JString ":error", JString "AttackingOwnUnits", cast units , cast target ]
  cast (NotInSupportRange units) = JArray [ JString ":error", JString "NotInSupportRange", cast units ]
  cast (NotSupportingUnits units) = JArray [ JString ":error", JString "NotSupportingUnits", cast units ]
  cast (NotInChainOfCommand units) = JArray [ JString ":error", JString "NotInChainOfCommand", cast units ]
  cast (NoSupplyColumnThere hex) = JArray [ JString ":error", JString "NoSupplyColumnThere", cast hex ]
  cast (NoStepsToLose side) = JArray [ JString ":error", JString "NoStepsToLose", cast side ]
  cast (CombatInProgress side) = JArray [ JString ":error", JString "CombatInProgress", cast side ]
  cast GameHasEnded = JArray [ JString ":error", JString "GameHasEnded" ]

export
Cast Losses JSON where
  cast (attackerLoss /> defenderLoss) = JArray [ cast attackerLoss, cast defenderLoss ]

export
Cast (GameUnit, Pos) JSON where
  cast (u, p) =
    JObject [ ("unit", cast u),
              ("pos", cast p)
              ]

export
Cast EngagedUnits JSON where
  cast (MkEngagedUnits base tacticalSupport strategicSupport) =
    JArray [ JString ":engaged"
          , JString ":base", cast base
          , JString ":tactical-support", cast tacticalSupport
          , JString ":strategic-support", cast strategicSupport
          ]

export
Cast CombatState JSON where
  cast (MkCombatState hex attackers defenders losses) =
    JArray [ JString ":combat-state"
          , JString ":combat-hex", cast hex
          , JString ":attackers", cast attackers
          , JString ":defenders", cast defenders
          , JString ":losses", cast losses
          ]

export
Cast CombatPhase JSON where
  cast NoCombat = JString "NoCombat"
  cast (AssignTacticalSupport side combat) = JArray [ JString "AssignTacticalSupport",  cast side, cast combat ]
  cast (AssignStrategicSupport side combat) = JArray [ JString "AssignStrategicSupport",  cast side, cast combat ]
  cast (ApplyLosses side combat) = JArray [ JString "ApplyLosses",  cast side, cast combat ]
  cast (Resolve combat) = JArray [ JString "Resolve", cast combat ]

export
Cast GameSegment JSON where
  cast Supply = JString "Supply"
  cast Move = JString "Move"
  cast (Combat phase) = JArray [ JString "Combat", cast phase ]
  cast GameEnd = JString "GameEnd"

export
Cast Event JSON where
  cast (Moved unit from to cost) =
      JArray [ JString ":moved"
            , JString ":unit", cast unit
            , JString ":from", cast from
            , JString ":to", cast to
            , JString ":cost", cast cost
            ]
  cast (CombatEngaged atk def target) =
      JArray [ JString ":combat-engaged"
            , JString ":attackers", cast atk
            , JString ":defenders", cast def
            , JString ":target", cast target
            ]
  cast (TacticalSupportProvided side units) =
      JArray [ JString ":tactical-support-provided"
            , JString ":supported-side", cast side
            , JString ":supporting-units", cast units
            ]
  cast (SupplyColumnUsed side hex) =
      JArray [ JString ":supply-column-used"
            , JString ":supported-side", cast side
            , JString ":position", cast hex
            ]
  cast (CombatResolved state losses) =
      JArray [ JString ":combat-resolved"
            , JString ":state", cast state
            , JString ":losses", cast losses
            ]
  cast (StepLost side unit remain) =
      JArray [ JString ":step-lost"
            , JString ":side", cast side
            , JString ":unit", cast unit
            , JString ":remaining-losses", cast remain
            ]
  cast (SegmentChanged from to) =
      JArray [ JString ":segment-changed"
            , JString ":from", cast from
            , JString ":to", cast to
            ]
  cast (TurnEnded n) =
      JArray [ JString ":turn-ended"
            , JString ":new-turn", cast n
            ]
  cast AxisTurnDone =
      JArray [ JString ":axis-turn-done" ]
  cast GameEnded =
      JArray [ JString ":game-ended" ]

export
splice : JSON -> JSON -> JSON
splice first (JArray xs) = JArray $ first :: xs
splice first s  = JArray [ first, s ]

export
Cast Terrain JSON where
  cast Clear = JString "Clear"
  cast Wood = JString "Wood"
  cast Rough = JString "Rough"
  cast RoughWood = JString "RoughWood"
  cast (Hill base) = splice (JString "Hill") (cast base)
  cast (Village base) = splice (JString "Village") (cast base)
  cast Town = JString "Town"
  cast (SupplySource side base) = splice (JArray [ JString "Supply", cast side] ) (cast base)

export
Cast Connection JSON where
  cast Plain = JString "Plain"
  cast (Road base) = splice (JString "Road") (cast base)
  cast (River base) = splice (JString "River") (cast base)
  cast Lake = JString "Lake"

export
Cast (Pos, Connection) JSON where
  cast (p, c) =
    JObject [ ("hex", cast p), ("link", cast c) ]

export
Cast (Pos, List (Pos, Connection)) JSON where
  cast (p, ns) =
    JObject [ ("hex", cast p), ("n", cast ns) ]

export
Cast Map JSON where
  cast (MkMap hexes edges) = JObject [ ("hexes", cast (tabulate hexes))
                                     , ("edges", cast edges)
                                     ]
export
Cast QueryError JSON where
  cast (NoSupplyPathFor unitName pos) = JObject [ ("reason", JString "NoSupplyPathFor"),
                                                ("unit", JString unitName),
                                                 ("pos" , cast pos) ]
  cast (UnitDoesNotExist unitName) = JObject [ ("reason", JString "UnitDoesNotExist"),
                                               ("unit" , JString unitName) ]

mutual
  private
  covering
  step : (x1 : List JSON) -> ((y : List JSON) -> Smaller y x1 -> Either String (List String)) -> Either String (List String)
  step []        f = Right []
  step (x :: xs) f =
     let lx = (length xs)
         prf = reflexive {rel = LTE}
     in do s <- toStrings x
           ss <- f xs $ LTESucc prf
           pure $ s ++ ss

  ||| Convert a s-expression into a list of strings
  |||
  ||| This function actually _flattens_ the given s-expression, traversing it depth-first and
  ||| expecting to find only _strings_.
  |||
  ||| * A single string is converted as a singleton
  ||| * A list of strings is converted as a list
  ||| * Any other type raises an error
  export
  -- It should be total but it is not due to the mutual recursion in the
  -- `step` function
  covering
  toStrings : JSON -> Either String (List String)
  toStrings (JString x) = pure [ x ]
  toStrings (JArray x) = sizeRec step x
  toStrings x = Left $ "Expected a string or a list of strings, got "++ show x
