module Bautzen.REPL.JSON

import Bautzen.Combats
import Bautzen.GameUnit
import Bautzen.Game
import Bautzen.Game.Core
import Bautzen.Pos as P
import Bautzen.Terrain
import Language.JSON
import Language.JSON.Data

import Data.Fin
import Data.Vect
import Control.WellFounded

import public JSON

%default total

export
Cast Side JSON where
  cast Axis = JString "Axis"
  cast Allies = JString "Allies"

export
FromJSON Side where
  fromJSON = withString "Side" $ \ s =>
    case s of
      "Axis" => pure Axis
      "Allies" => pure Allies
      _ => fail $ "Unknown side: " ++ s

export
makeSide : JSON -> Either String Side
makeSide = mapFst show . fromJSON

export
Cast Nation JSON where
  cast German = JString "German"
  cast Russian = JString "Russian"
  cast Polish = JString "Polish"

export
FromJSON Nation where
  fromJSON = withString "Nation" $ \s =>
    case s of
     "German" => pure German
     "Russian" => pure Russian
     "Polish" => pure Polish
     _ => fail $ "Unknown nation " ++ s

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
FromJSON UnitType where
  fromJSON = withString "UnitType" $ \ s =>
    case s of
       "Armored" => pure Armored
       "HeavyArmored" => pure HeavyArmored
       "MechInfantry" => pure MechInfantry
       "Infantry" => pure Infantry
       "HeavyEngineer" => pure HeavyEngineer
       "Artillery" => pure Artillery
       "AntiTank" => pure AntiTank
       "HQ" => pure HQ
       "SupplyColumn" => pure SupplyColumn
       _ => fail $ "Unknown UnitType: "++ s

export
Cast UnitSize JSON where
  cast Regiment = JString "Regiment"
  cast Brigade = JString "Brigade"
  cast Division = JString "Division"
  cast Corps = JString "Corps"
  cast Army = JString "Army"

export
FromJSON UnitSize where
  fromJSON = withString "UnitSize" $ \ s =>
    case s of
       "Regiment" => pure Regiment
       "Brigade" => pure Brigade
       "Division" => pure Division
       "Corps" => pure Corps
       "Army" => pure Army
       _ => fail $ "Unknown UnitSize: " ++ s

export
Cast Nat JSON where
  cast = cast . cast {to = Double} . natToInteger

export
Cast StdFactors JSON where
  cast (MkStdFactors attack defense) =
    JObject [ ("tag", JString "StdFactors")
            , ("attack", cast attack)
            , ("defense", cast defense)
            ]

export
FromJSON StdFactors where
  fromJSON = withObject "StdFactors" $ \ obj =>
     [| MkStdFactors (obj .: "attack") (obj .: "defense") |]

export
Cast Arty JSON where
  cast (MkArty support distance) =
    JObject [ ("tag", JString "Arty")
            , ("support", cast support)
            , ("distance", cast distance)
            ]

export
FromJSON Arty where
  fromJSON = withObject "Arty" $ \ obj =>
     [| MkArty (obj .: "support") (obj .: "distance") |]

export
Cast Pak JSON where
  cast (MkPak antitank) =
      JObject [ ("tag", JString "Pak")
              , ("antitank", cast antitank)
              ]

export
FromJSON Pak where
  fromJSON = withObject "Arty" $ \ obj =>
     MkPak <$> (obj .: "antitank")

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
    JObject [ ("nation", cast nation)
            , ("type", cast unitType)
            , ("name", cast name)
            , ("parent", cast parent)
            , ("size", cast size)
            , ("move", cast move)
            , ("mp", cast currentMP)
            , ("steps", cast steps)
            , ("hits", cast hits)
            , ("combat", case unitType of
                Armored       => cast combat
                HeavyArmored  => cast combat
                MechInfantry  => cast combat
                Infantry      => cast combat
                HeavyEngineer => cast combat
                Artillery     => cast combat
                AntiTank      => cast combat
                HQ            => cast combat
                SupplyColumn  => cast combat)
            ]

parseFin : (n : Nat) -> (x : Nat) -> Either JSONErr (Fin n)
parseFin n x with (natToFin x n)
  parseFin n x | (Just k) = pure k
  parseFin n x | Nothing = fail #"number greater than bound \#{show n}"#

parseFactors : Value v obj => (steps : Nat) -> (unitType : UnitType) -> Parser v (Vect steps (Factors unitType))
parseFactors s Armored = fromJSON
parseFactors s HeavyArmored = fromJSON
parseFactors s MechInfantry = fromJSON
parseFactors s Infantry = fromJSON
parseFactors s HeavyEngineer = fromJSON
parseFactors s Artillery = fromJSON
parseFactors s AntiTank = fromJSON
parseFactors s HQ = fromJSON
parseFactors s SupplyColumn = fromJSON

export
FromJSON GameUnit where
  fromJSON = withObject "GameUnit" $ \ obj => do
    nation <- (obj .: "nation")
    unitType <- (obj .: "type")
    name <- (obj .: "name")
    parent <- (obj .: "parent")
    size <- (obj .: "size")
    move <- (obj .: "move")
    mp <- (obj .: "mp")
    steps <- (obj .: "steps")
    hits <- parseFin steps =<< (obj .: "hits")
    combat <- explicitParseField (parseFactors steps unitType) obj "combat"
    pure $ MkGameUnit  nation unitType name parent size move mp steps hits combat

public export
Cast (P.Loc c r) JSON where
  cast (P.Hex col row) =
    JArray [ cast col
           , cast row
           ]

export
FromJSON Pos where
  fromJSON = withArray "Pos" $ \ l =>
    case l of
       [x,y] => [| hex (parseFin 23 =<< fromJSON x) (parseFin 13 =<< fromJSON y) |]
       _ => fail $ "Expected list of 2 integers"

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
  cast (InvalidPlacement pos) = JArray [ JString ":error", JString "InvalidPlacement", cast pos ]
  cast GameHasEnded = JArray [ JString ":error", JString "GameHasEnded" ]

export
Cast Losses JSON where
  cast (attackerLoss /> defenderLoss) = JArray [ cast attackerLoss, cast defenderLoss ]

export
FromJSON Losses where
  fromJSON = withArray "Losses" $ \ ls =>
     case ls of
       [ att, def ] => [| (/>) (fromJSON att) (fromJSON def) |]
       otherwise => fail $ "Wrong number of elements, expected 2"

export
Cast (GameUnit, Pos) JSON where
  cast (u, p) =
    JObject [ ("unit", cast u),
              ("pos", cast p)
              ]

export
Cast AllPositions JSON where
  cast (MkAllPositions positions)  =
    JObject [ ("tag", cast "Positions"),
              ("positions", cast positions)
              ]

export
Cast EngagedUnits JSON where
  cast (MkEngagedUnits base tacticalSupport strategicSupport) =
    JObject [ ("tag", JString "EngagedUnits")
            , ("base", cast base)
            , ("tacticalSupport", cast tacticalSupport)
            , ("strategicSupport", cast strategicSupport)
            ]

export
FromJSON EngagedUnits where
  fromJSON = withObject "EngagedUnits" $ \ obj =>
     [| MkEngagedUnits (obj .: "base") (obj .: "tacticalSupport") (obj .: "strategicSupport") |]

export
Cast CombatState JSON where
  cast (MkCombatState hex attackers defenders losses) =
    JObject  [ ("combatHex", cast hex)
             , ("attackers", cast attackers)
             , ("defenders", cast defenders)
             , ("losses", cast losses)
             ]

export
FromJSON CombatState where
  fromJSON = withObject "CombatState" $ \ obj =>
      [| MkCombatState (obj .: "combatHex") (obj .: "attackers") (obj .: "defenders") (obj .: "losses") |]

export
Cast CombatPhase JSON where
  cast NoCombat = JObject [("tag", JString "NoCombat")]
  cast (AssignTacticalSupport side combat) = JObject  [ ("tag", JString "AssignTacticalSupport"),  ("side", cast side), ("combat", cast combat) ]
  cast (AssignStrategicSupport side combat) = JObject [ ("tag",  JString "AssignStrategicSupport"),  ("side", cast side), ("combat", cast combat) ]
  cast (ApplyLosses side combat) = JObject [ ("tag",  JString "ApplyLosses"),  ("side", cast side), ("combat", cast combat) ]
  cast (Resolve combat) = JObject [ ("tag", JString "Resolve"),("combat",  cast combat) ]

export
FromJSON CombatPhase where
  fromJSON = withObject "CombatPhase" $ \ obj => do
     tag <- (.:) {a = String} obj "tag"
     case tag of
       "NoCombat" => pure NoCombat
       "AssignStrategicSupport" => [| AssignStrategicSupport (obj .: "side") (obj .: "combat") |]
       "AssignTacticalSupport" => [| AssignTacticalSupport (obj .: "side") (obj .: "combat") |]
       "ApplyLosses" =>  [| ApplyLosses (obj .: "side") (obj .: "combat") |]
       "Resolve" =>  Resolve <$> (obj .: "combat")
       _ => fail $ "Unknown tag " ++ tag

export
Cast GameSegment JSON where
  cast Setup = JObject [ ("tag", JString "Setup")]
  cast Supply = JObject [ ("tag", JString "Supply")]
  cast Move = JObject [ ("tag", JString "Move")]
  cast (Combat phase) = JObject [ ("tag", JString "Combat"), ("phase", cast phase) ]
  cast GameEnd = JObject [ ("tag", JString "GameEnd")]

export
FromJSON GameSegment where
  fromJSON = withObject "GameSegment" $ \ obj => do
     tag <- (.:) {a = String} obj "tag"
     case tag of
        "Setup" => pure Setup
        "Supply" => pure Supply
        "Move" => pure Move
        "Combat" => Combat <$> (obj .: "phase")
        "GameEnd" => pure GameEnd
        _ => fail $ "Unknown tag " ++ tag

export
Cast (Event seg) JSON where
  cast (Placed unit pos) =
      JObject [ ("tag", JString "Placed")
            , ("unit", JString $ fullName unit)
            , ("pos", cast pos)
            ]
  cast (Moved unit from to cost) =
      JObject [ ("tag", JString "Moved")
            , ("unit", JString $ fullName unit)
            , ("from", cast from)
            , ("to", cast to)
            , ("cost", cast cost)
            ]
  cast (CombatEngaged atk def target) =
      JObject [ ("tag", JString "CombatEngaged")
            , ("attackers", cast atk)
            , ("defenders", cast def)
            , ("target", cast target)
            ]
  cast (TacticalSupportProvided side units) =
      JObject [ ("tag", JString "tactical-support-provided")
            , ("supported-side", cast side)
            , ("supporting-units", cast units)
            ]
  cast (SupplyColumnUsed side hex) =
      JObject [ ("tag" , JString "supply-column-used")
            , ("supported-side", cast side)
            , ("position", cast hex)
            ]
  cast (CombatResolved state losses) =
      JObject [ ("tag", JString  "combat-resolved")
            , ("state", cast state)
            , ("losses", cast losses)
            ]
  cast (StepLost side unit remain) =
      JObject [ ("tag", JString  "step-lost")
            , ("side", cast side)
            , ("unit", cast unit)
            , ("remaining-losses", cast remain)
            ]
  cast (SegmentChanged from to) =
      JObject [ ("tag", JString "SegmentChanged")
            , ("from", cast from)
            , ("to", cast to)
            ]
  cast (TurnEnded n) =
      JObject [ ("tag", JString "turn-ended")
            , ("new-turn", cast n)
            ]
  cast AxisTurnDone =
      JObject [ ("tag", JString "axis-turn-done") ]
  cast AlliesSetupDone =
      JObject [ ("tag", JString "AlliesSetupDone") ]
  cast GameEnded =
      JObject [ ("tag", JString "game-ended") ]

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
  cast (MkMap hexes edges) = JObject [ ("tag", JString "Map")
                                     , ("hexes", cast (tabulate hexes))
                                     , ("edges", cast edges)
                                     ]

export
Cast CurrentGameSegment JSON where
  cast (MkCurrentGameSegment turn side segment) =
    JObject [ ("tag", JString "CurrentGameSegment")
            , ("turn", cast turn)
            , ("side", cast side)
            , ("segment", cast segment)
            ]

export
Cast QueryError JSON where
  cast (NoSupplyPathFor unitName pos) = JObject [ ("reason", JString "NoSupplyPathFor"),
                                                ("unit", JString unitName),
                                                 ("pos" , cast pos) ]
  cast (UnitDoesNotExist unitName) = JObject [ ("reason", JString "UnitDoesNotExist"),
                                               ("unit" , JString unitName) ]
export
Cast (ActionResult seg) JSON where
  cast (ResEvent x) = cast x
  cast (ResError x) = cast x
  cast (ResQuery x) = cast x

export
Cast GameState JSON where
  cast (MkGameState turn side stateSegment units) =
    JObject [ ("tag", JString "GameState")
            , ("turn" , cast turn)
            , ("side", cast side)
            , ("stateSegment", cast stateSegment)
            , ("units", cast units)
            ]

export
Cast Game JSON where
  cast (MkGame curState gameMap) =
    JObject [ ("tag", JString "Game")
            , ("curState", cast curState)
            , ("gameMap" , cast gameMap)
            ]

||| Convert a JSON into a list of strings
|||
||| This function actually _flattens_ the given s-expression, traversing it depth-first and
||| expecting to find only _strings_.
|||
||| * A single string is converted as a singleton
||| * A list of strings is converted as a list
||| * Any other type raises an error
toStrings : JSON -> Either String (List String)

step : List JSON -> Either String (List String)
step [] = pure []
step (x :: xs) = [| toStrings x ++ step xs |]

toStrings (JString str) = pure [str]
toStrings (JArray xs) = step xs
toStrings x = Left $ "Expected a string or a list of strings, got "++ show x

makePos : (col : Int) -> (row : Int) -> Either String Pos
makePos col row with (integerToNat (cast col), integerToNat (cast row))
  makePos col row | (c , r) with (natToFin c 23, natToFin r 13)
    makePos col row | (c , r)  | (Just c', Just r') = Right $ hex c' r'
    makePos col row | (c , r)  | _              = Left $ "position should be between (0,0) and (22, 12): " ++ show col ++ ", " ++ show row

makeMoveCommand : (unitName : String) -> (col : Int) -> (row : Int) -> Either String (Command Move)
makeMoveCommand unitName col row = MoveTo unitName <$> makePos col row

makeAttackWithCommand : (unitNames : JSON) -> (col : Int) -> (row : Int) -> Either String (Command $ Combat NoCombat)
makeAttackWithCommand unitNames col row = do
  pos <- makePos col row
  units <- toStrings unitNames
  pure $ AttackWith units pos

makeSupportCommand : (unitNames : JSON) -> {side : Side} -> {combatState : CombatState}
                   -> Either String (Command $ Combat (AssignTacticalSupport side combatState))
makeSupportCommand unitNames =
  toStrings unitNames >>= Right . TacticalSupport

makeLoseStepCommand : (unitName : String) -> {side : Side} -> {combatState : CombatState}
                   -> Either String (Command $ Combat (ApplyLosses side combatState))
makeLoseStepCommand unitName = pure $ LoseStep unitName

export
makePlayerAction : (game : Game) -> JSON -> Either String (PlayerAction (curSegment game))
makePlayerAction game (JObject [ ("tag", JString "MoveTo"), ("unit", JString unitName), ("to", JArray [ JNumber col, JNumber row]) ] ) with (curSegment game)
  makePlayerAction game (JObject [ ("tag", JString "MoveTo"), ("unit", JString unitName), ("to", JArray [ JNumber col, JNumber row]) ] ) | Move = Cmd <$> makeMoveCommand unitName (cast col) (cast row)
  makePlayerAction game (JObject [ ("tag", JString "MoveTo"), ("unit", JString unitName), ("to", JArray [ JNumber col, JNumber row]) ] ) | other = Left ("Invalid command for segment " ++ show other)
makePlayerAction game (JObject [ ("tag", JString "Attack"), ("units", unitNames), ("hex", JArray [ JNumber col, JNumber row]) ] ) with (curSegment game)
  makePlayerAction game (JObject [ ("tag", JString "Attack"), ("units", unitNames), ("hex", JArray [ JNumber col, JNumber row]) ] ) | Combat NoCombat = Cmd <$> makeAttackWithCommand unitNames (cast col) (cast row)
  makePlayerAction game (JObject [ ("tag", JString "Attack"), ("units", unitNames), ("hex", JArray [ JNumber col, JNumber row]) ] ) | other = Left $ "Invalid command for segment " ++ show other
makePlayerAction game (JArray [ JString "support!", unitNames ] ) with (curSegment game)
  makePlayerAction game (JArray [ JString "support!", unitNames ] ) | Combat (AssignTacticalSupport side combatState) = Cmd <$> makeSupportCommand unitNames
  makePlayerAction game (JArray [ JString "support!", unitNames ] ) | other = Left $ "Invalid command for segment " ++ show other
makePlayerAction game (JArray [ JString "resolve!" ] ) with (curSegment game)
  makePlayerAction game (JArray [ JString "resolve!" ] ) | Combat (Resolve combatState) = pure $ Cmd (ResolveCombat combatState)
  makePlayerAction game (JArray [ JString "resolve!" ] ) | other = Left $ "Invalid command for segment " ++ show other
makePlayerAction game (JArray [ JString "lose-step!", JString unitName ] ) with (curSegment game)
  makePlayerAction game (JArray [ JString "lose-step!", JString unitName ] ) | Combat (ApplyLosses side state) = Cmd <$> makeLoseStepCommand unitName
  makePlayerAction game (JArray [ JString "lose-step!", JString unitName ] ) | other = Left $ "Invalid command for segment " ++ show other
makePlayerAction game (JObject [("tag", JString  "Next")] ) = pure $ Cmd NextSegment
makePlayerAction game (JArray [ JString "supply-path?", JString unitName ] ) = Right $ Qry $ SupplyPath unitName
makePlayerAction game (JArray [ JString "map?" ] ) = Right $ Qry TerrainMap
makePlayerAction game (JArray [ JString "positions?" ] ) = Right $ Qry Positions
makePlayerAction game (JObject [("tag", JString  "Place"), ("unitName", JString unitName), ("position", JArray [ JNumber col, JNumber row] )]) with (curSegment game)
  makePlayerAction game (JObject [("tag", JString  "Place"), ("unitName", JString unitName), ("position", JArray [ JNumber col, JNumber row] )]) | Setup = makePos (cast col) (cast row) >>=   pure . Cmd . Place unitName
  makePlayerAction game (JObject [("tag", JString  "Place"), ("unitName", JString unitName), ("position", JArray [ JNumber col, JNumber row] )]) | other  = Left ("Invalid command for segment " ++ show other)
makePlayerAction game (JObject (("tag", JString  "GetCurrentSegment") :: _)) = Right $ Qry GetCurrentSegment
makePlayerAction _ sexp = Left $ "Unknown command " ++ show @{Idris} sexp
