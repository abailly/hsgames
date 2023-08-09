module Bautzen.REPL.JSON

import Bautzen.Combats
import Bautzen.GameUnit
import Bautzen.Game
import Bautzen.Game.Core
import Bautzen.Pos as P
import Bautzen.Terrain
import JSON.Parser

import Data.Fin
import Data.Vect
import Control.WellFounded

import public JSON
import public JSON.ToJSON

%hide Text.Lex.Shift.string
%default total

export
ToJSON Side where
  toJSON Axis = string "Axis"
  toJSON Allies = string "Allies"

export
FromJSON Side where
  fromJSON = withString "Side" $ \ s =>
    case s of
      "Axis" => pure Axis
      "Allies" => pure Allies
      _ => fail $ "Unknown side: " ++ s

export
makeSide : String -> Either String Side
makeSide s =
    case s of
      "Axis" => Right Axis
      "Allies" => Right Allies
      _ => Left $ "Unknown side: " ++ s

export
ToJSON Nation where
  toJSON German = string "German"
  toJSON Russian = string "Russian"
  toJSON Polish = string "Polish"

export
FromJSON Nation where
  fromJSON = withString "Nation" $ \s =>
    case s of
     "German" => pure German
     "Russian" => pure Russian
     "Polish" => pure Polish
     _ => fail $ "Unknown nation " ++ s

export
ToJSON UnitType where
  toJSON Armored = string "Armored"
  toJSON HeavyArmored = string "HeavyArmored"
  toJSON MechInfantry = string "MechInfantry"
  toJSON Infantry = string "Infantry"
  toJSON HeavyEngineer = string "HeavyEngineer"
  toJSON Artillery = string "Artillery"
  toJSON AntiTank = string "AntiTank"
  toJSON HQ = string "HQ"
  toJSON SupplyColumn = string "SupplyColumn"

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
ToJSON UnitSize where
  toJSON Regiment = string "Regiment"
  toJSON Brigade = string "Brigade"
  toJSON Division = string "Division"
  toJSON Corps = string "Corps"
  toJSON Army = string "Army"

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
ToJSON StdFactors where
  toJSON (MkStdFactors attack defense) =
    object [ ("tag", string "StdFactors")
            , ("attack", toJSON attack)
            , ("defense", toJSON defense)
            ]

export
FromJSON StdFactors where
  fromJSON = withObject "StdFactors" $ \ obj =>
     [| MkStdFactors (field obj "attack") (field obj "defense") |]

export
ToJSON Arty where
  toJSON (MkArty support distance) =
    object [ ("tag", string "Arty")
            , ("support", toJSON support)
            , ("distance", toJSON distance)
            ]

export
FromJSON Arty where
  fromJSON = withObject "Arty" $ \ obj =>
     [| MkArty (field obj "support") (field obj "distance") |]

export
ToJSON Pak where
  toJSON (MkPak antitank) =
      object [ ("tag", string "Pak")
              , ("antitank", toJSON antitank)
              ]

export
FromJSON Pak where
  fromJSON = withObject "Arty" $ \ obj =>
     MkPak <$> (field obj "antitank")

export
ToJSON (Fin n) where
  toJSON = toJSON { a = Integer} . finToInteger

export
ToJSON a => ToJSON b => ToJSON c => ToJSON (a, b, c) where
  toJSON (a,b,c) = array [ toJSON a, toJSON b, toJSON c ]

export
ToJSON GameUnit where
  toJSON (MkGameUnit nation unitType name parent size move currentMP steps hits combat) =
    object [ ("nation", toJSON nation)
            , ("type", toJSON unitType)
            , ("name", toJSON name)
            , ("parent", toJSON parent)
            , ("size", toJSON size)
            , ("move", toJSON move)
            , ("mp", toJSON currentMP)
            , ("steps", toJSON steps)
            , ("hits", toJSON hits)
            , ("combat", case unitType of
                Armored       => toJSON combat
                HeavyArmored  => toJSON combat
                MechInfantry  => toJSON combat
                Infantry      => toJSON combat
                HeavyEngineer => toJSON combat
                Artillery     => toJSON combat
                AntiTank      => toJSON combat
                HQ            => toJSON combat
                SupplyColumn  => toJSON combat)
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
    nation <- (field obj "nation")
    unitType <- (field obj "type")
    name <- (field obj "name")
    parent <- (field obj "parent")
    size <- (field obj "size")
    move <- (field obj "move")
    mp <- (field obj "mp")
    steps <- (field obj "steps")
    hits <- parseFin steps =<< (field obj "hits")
    combat <- explicitParseField (parseFactors steps unitType) obj "combat"
    pure $ MkGameUnit  nation unitType name parent size move mp steps hits combat

public export
ToJSON (P.Loc c r) where
  toJSON (P.Hex col row) =
    array [ toJSON col
           , toJSON row
           ]

export
FromJSON Pos where
  fromJSON = withArray "Pos" $ \ l =>
    case l of
       [x,y] => [| hex (parseFin 23 =<< fromJSON x) (parseFin 13 =<< fromJSON y) |]
       _ => fail $ "Expected list of 2 integers"

export
ToJSON Pos where
  toJSON (MkPos p) = toJSON p

export
ToJSON GameError where
  toJSON (NoSuchUnits unitNames) = object [ ("tag", string "NoSuchUnit"), ("unitNames", toJSON unitNames) ]
  toJSON (NotYourTurn side) = object [ ("tag", string "NotYourTurn"), ( "side", toJSON side) ]
  toJSON (EnemyInHex unit hex) = object [ ("tag", string "EnemyInHex"), ("unit", toJSON unit), ("hex", toJSON hex) ]
  toJSON (MoveFromZocToZoc unit to) = object [ ("tag", string "MoveFromZocToZoc"), ("unit", toJSON unit), ("to", toJSON to) ]
  toJSON (ForbiddenTerrain from to) = object [ ("tag", string "ForbiddenTerrain"), ("from", toJSON from), ("to", toJSON to) ]
  toJSON (InvalidMove from to) = object [ ("tag", string "InvalidMove"), ("from", toJSON from) , ( "to", toJSON to) ]
  toJSON (NotEnoughMPs unit from to mp) = object [ ("tag", string "NotEnoughMPs"), ("unit", toJSON unit), ("from", toJSON from) , ( "to", toJSON to), ( "mp", toJSON mp) ]
  toJSON (NotAdjacentTo units to) = object [ ("tag", string "NotAdjacentTo"), ("units", toJSON units) , ("to", toJSON to)  ]
  toJSON (NothingToAttack target) = object [ ("tag", string "NothingToAttack"), ( "target", toJSON target) ]
  toJSON (AttackingOwnUnits units target) = object [ ("tag", string "AttackingOwnUnits"), ("units", toJSON units) , ( "target", toJSON target) ]
  toJSON (NotInSupportRange units) = object [ ("tag", string "NotInSupportRange"), ("units", toJSON units) ]
  toJSON (NotSupportingUnits units) = object [ ("tag", string "NotSupportingUnits"), ("units", toJSON units) ]
  toJSON (NotInChainOfCommand units) = object [ ("tag", string "NotInChainOfCommand"), ("units", toJSON units) ]
  toJSON (NoSupplyColumnThere hex) = object [ ("tag", string "NoSupplyColumnThere"), ("hex", toJSON hex)  ]
  toJSON (NoStepsToLose side) = object [ ("tag", string "NoStepsToLose"), ( "side", toJSON side) ]
  toJSON (CombatInProgress side) = object [ ("tag", string "CombatInProgress"), ( "side", toJSON side) ]
  toJSON (InvalidPlacement pos) = object [ ("tag", string "InvalidPlacement"), ("pos", toJSON pos) ]
  toJSON GameHasEnded = object [ ("tag", string "GameHasEnded") ]

export
FromJSON GameError where
  fromJSON = withObject "GameError" $ \ o => do
     tag <- field o "tag"
     case the String tag of
       "NoSuchUnits" => [| NoSuchUnits (field o "unitNames") |]
       "NotYourTurn" => [| NotYourTurn (field o "side") |]
       "EnemyInHex"  => [| EnemyInHex (field o "unit") (field o "hex") |]
       "MoveFromZocToZoc"  => [| MoveFromZocToZoc (field o "unit") (field o "to")|]
       "ForbiddenTerrain"  => [| ForbiddenTerrain (field o "from") (field o "to")|]
       "InvalidMove"  => [| InvalidMove (field o "from") (field o "to")|]
       "NotEnoughMPs"  => [| NotEnoughMPs (field o "unit") (field o "from") (field o "to") (field o "mp")|]
       "NotAdjacentTo"  => [| NotAdjacentTo (field o "units") (field o "to")|]
       "NothingToAttack"  => [| NothingToAttack (field o "target") |]
       "AttackingOwnUnits"  => [| AttackingOwnUnits (field o "units") (field o "target") |]
       "NotInSupportRange" => [| NotInSupportRange (field o "units")|]
       "NotSupportingUnits"  => [| NotSupportingUnits (field o "units")|]
       "NotInChainOfCommand"  => [| NotInChainOfCommand (field o "units")|]
       "NoSupplyColumnThere"  => [| NoSupplyColumnThere (field o "hex") |]
       "NoStepsToLose" => [| NoStepsToLose (field o "side") |]
       "CombatInProgress" => [| CombatInProgress (field o  "side") |]
       "InvalidPlacement" => [| InvalidPlacement (field o "pos") |]
       "GameHasEnded"  => pure GameHasEnded
       _ => fail #"unknown error tag #{tag}"#

export
ToJSON Losses where
  toJSON (attackerLoss /> defenderLoss) = array [ toJSON attackerLoss, toJSON defenderLoss ]

export
FromJSON Losses where
  fromJSON = withArray "Losses" $ \ ls =>
     case ls of
       [ att, def ] => [| (/>) (fromJSON att) (fromJSON def) |]
       otherwise => fail $ "Wrong number of elements, expected 2"

export
ToJSON AllPositions where
  toJSON (MkAllPositions positions)  =
    object [ ("tag", toJSON "Positions"),
             ("positions", toJSON positions)
           ]

export
ToJSON EngagedUnits where
  toJSON (MkEngagedUnits base tacticalSupport strategicSupport) =
    object [ ("tag", string "EngagedUnits")
            , ("base", toJSON base)
            , ("tacticalSupport", toJSON tacticalSupport)
            , ("strategicSupport", toJSON strategicSupport)
            ]

export
FromJSON EngagedUnits where
  fromJSON = withObject "EngagedUnits" $ \ obj =>
     [| MkEngagedUnits (field obj "base") (field obj "tacticalSupport") (field obj "strategicSupport") |]

export
ToJSON CombatState where
  toJSON (MkCombatState hex attackers defenders losses) =
    object  [ ("combatHex", toJSON hex)
             , ("attackers", toJSON attackers)
             , ("defenders", toJSON defenders)
             , ("losses", toJSON losses)
             ]

export
FromJSON CombatState where
  fromJSON = withObject "CombatState" $ \ obj =>
      [| MkCombatState (field obj "combatHex") (field obj "attackers") (field obj "defenders") (field obj "losses") |]

export
ToJSON CombatPhase where
  toJSON NoCombat = object [("tag", string "NoCombat")]
  toJSON (AssignTacticalSupport side combat) = object  [ ("tag", string "AssignTacticalSupport"),  ("side", toJSON side), ("combat", toJSON combat) ]
  toJSON (AssignStrategicSupport side combat) = object [ ("tag",  string "AssignStrategicSupport"),  ("side", toJSON side), ("combat", toJSON combat) ]
  toJSON (ApplyLosses side combat) = object [ ("tag",  string "ApplyLosses"),  ("side", toJSON side), ("combat", toJSON combat) ]
  toJSON (Resolve combat) = object [ ("tag", string "Resolve"),("combat",  toJSON combat) ]

export
FromJSON CombatPhase where
  fromJSON = withObject "CombatPhase" $ \ obj => do
     tag <- field {a = String} obj "tag"
     case tag of
       "NoCombat" => pure NoCombat
       "AssignStrategicSupport" => [| AssignStrategicSupport (field obj "side") (field obj "combat") |]
       "AssignTacticalSupport" => [| AssignTacticalSupport (field obj "side") (field obj "combat") |]
       "ApplyLosses" =>  [| ApplyLosses (field obj "side") (field obj "combat") |]
       "Resolve" =>  Resolve <$> (field obj "combat")
       _ => fail $ "Unknown tag " ++ tag

export
ToJSON GameSegment where
  toJSON Setup = object [ ("tag", string "Setup")]
  toJSON Supply = object [ ("tag", string "Supply")]
  toJSON Move = object [ ("tag", string "Move")]
  toJSON (Combat phase) = object [ ("tag", string "Combat"), ("phase", toJSON phase) ]
  toJSON GameEnd = object [ ("tag", string "GameEnd")]

export
FromJSON GameSegment where
  fromJSON = withObject "GameSegment" $ \ obj => do
     tag <- field {a = String} obj "tag"
     case tag of
        "Setup" => pure Setup
        "Supply" => pure Supply
        "Move" => pure Move
        "Combat" => Combat <$> (field obj "phase")
        "GameEnd" => pure GameEnd
        _ => fail $ "Unknown tag " ++ tag

export
ToJSON (Event seg) where
  toJSON (Placed unit pos) =
      object [ ("tag", string "Placed")
              , ("unit", toJSON unit)
              , ("pos", toJSON pos)
              ]
  toJSON (Moved unit from to cost) =
      object [ ("tag", string "Moved")
            , ("unit", toJSON unit)
            , ("from", toJSON from)
            , ("to", toJSON to)
            , ("cost", toJSON cost)
            ]
  toJSON (CombatEngaged atk def target) =
      object [ ("tag", string "CombatEngaged")
            , ("attackers", toJSON atk)
            , ("defenders", toJSON def)
            , ("target", toJSON target)
            ]
  toJSON (TacticalSupportProvided side units) =
      object [ ("tag", string "TacticalSupportProvided")
            , ("supportedSide", toJSON side)
            , ("supportingUnits", toJSON units)
            ]
  toJSON (SupplyColumnUsed side hex) =
      object [ ("tag" , string "SupplyColumnUsed")
            , ("supportedSide", toJSON side)
            , ("position", toJSON hex)
            ]
  toJSON (CombatResolved state losses) =
      object [ ("tag", string  "CombatResolved")
            , ("state", toJSON state)
            , ("losses", toJSON losses)
            ]
  toJSON (StepLost side unit remain) =
      object [ ("tag", string  "StepLost")
            , ("side", toJSON side)
            , ("unit", toJSON unit)
            , ("remainingLosses", toJSON remain)
            ]
  toJSON (SegmentChanged from to) =
      object [ ("tag", string "SegmentChanged")
            , ("from", toJSON from)
            , ("to", toJSON to)
            ]
  toJSON (TurnEnded n) =
      object [ ("tag", string "TurnEnded")
            , ("newTurn", toJSON n)
            ]
  toJSON AxisTurnDone =
      object [ ("tag", string "AxisTurnDone") ]
  toJSON AlliesSetupDone =
      object [ ("tag", string "AlliesSetupDone") ]
  toJSON GameEnded =
      object [ ("tag", string "game-ended") ]

ToJSON AnyEvent where
  toJSON (MkAnyEvent {seg} e) =
    object [("segment", toJSON seg), ("event", toJSON e)]


partial
parseAnyEvent : Value v obj => GameSegment -> Parser v AnyEvent
parseAnyEvent seg = withObject "Event" $ \ o => do
     tag <- field o "tag"
     case the String tag of
       "Placed" => MkAnyEvent <$> [| Placed (field o "unit") (field o "pos") |]
       "Moved" => do
          unit <- field o "unit"
          from <- (field o "from")
          to <- (field o "to")
          cost <- (field o "cost")
          case isLTE (toNat cost) (currentMP unit) of
             Yes prf => pure $ MkAnyEvent $ Moved unit from to cost
             No _ => fail #"Invalid Moved event, cost (#{show cost}) is greater than current MP (#{show $ currentMP unit })"#
       "CombatEngaged" => do
          atts <- field o "attackers"
          defs <- field o "defenders"
          tgt <- field o "target"
          pure $ MkAnyEvent $ CombatEngaged atts defs tgt
       "TacticalSupportProvided" => do
          supportingUnits <- field o "supportingUnits"
          let sup = the (List (GameUnit, Pos)) supportingUnits
          case seg of
            t@(Combat (AssignTacticalSupport supportedSide st)) =>
               let e : Event t
                   e = TacticalSupportProvided supportedSide sup
               in  pure $ MkAnyEvent e
            other => fail "inconsistent combat phase for tactical support"
       "SupplyColumnUsed" => do
          position <- field o "position"
          let pos = the Pos position
          case seg of
            t@(Combat (AssignStrategicSupport supportedSide st)) =>
               let e : Event t
                   e = SupplyColumnUsed supportedSide pos
               in  pure $ MkAnyEvent e
            other => fail "inconsistent combat phase for strategic support"
       "CombatResolved" => do
          ls <- field o "losses"
          let losses = the Losses ls
          case seg of
            Combat (Resolve st') =>
               pure $ MkAnyEvent $ CombatResolved st' losses
            other => fail "inconsistent combat phase for resolve combat"
       "StepLost" => do
          unit <- field o "unit"
          remain <- field o "remainingLosses"
          let u = the GameUnit unit
          let r = the Losses remain
          case seg of
            t@(Combat (ApplyLosses side combatState)) =>
               let e : Event t
                   e = StepLost side u r
               in  pure $ MkAnyEvent e
            other => fail "inconsistent combat phase for step lost"
       "SegmentChanged" => do
         from <- field o "from"
         to <- field o "to"
         pure $ MkAnyEvent $ SegmentChanged from to
       "TurnEnded" => do
         newTurn <- parseFin 6 =<< field o "newTurn"
         pure $ MkAnyEvent $ TurnEnded newTurn
       "AxisTurnDone" => pure $ MkAnyEvent AxisTurnDone
       "AlliesSetupDone" => pure $ MkAnyEvent AlliesSetupDone
       "GameEnded"  =>
          pure $ MkAnyEvent {seg} GameEnded
       other => fail #"Unknown tag for Event: #{tag}"#

partial
export
FromJSON AnyEvent where
  fromJSON = withObject "Event" $ \ any => do
    seg <- field any "segment"
    explicitParseField (parseAnyEvent seg) any "event"

export
ToJSON Terrain where
  toJSON Clear = string "Clear"
  toJSON Wood = string "Wood"
  toJSON Rough = string "Rough"
  toJSON RoughWood = string "RoughWood"
  toJSON (Hill base) = array [(string "Hill"),  (toJSON base)]
  toJSON (Village base) = array [(string "Village") , (toJSON base) ]
  toJSON Town = string "Town"
  toJSON (SupplySource side base) = array [toJSON ( "Supply" ,side), (toJSON base) ]

export
ToJSON Connection where
  toJSON Plain = string "Plain"
  toJSON (Road base) = array [ (string "Road") , (toJSON base)]
  toJSON (River base) = array [ (string "River"),  (toJSON base) ]
  toJSON Lake = string "Lake"

export
ToJSON Map where
  toJSON (MkMap hexes edges) = object [ ("tag", string "Map")
                                     , ("hexes", toJSON (tabulate hexes))
                                     , ("edges", toJSON edges)
                                     ]

export
ToJSON CurrentGameSegment where
  toJSON (MkCurrentGameSegment turn side segment) =
    object [ ("tag", string "CurrentGameSegment")
            , ("turn", toJSON turn)
            , ("side", toJSON side)
            , ("segment", toJSON segment)
            ]

export
ToJSON QueryError where
  toJSON (NoSupplyPathFor unitName pos) = object [ ("reason", string "NoSupplyPathFor"),
                                                ("unit", string unitName),
                                                 ("pos" , toJSON pos) ]
  toJSON (UnitDoesNotExist unitName) = object [ ("reason", string "UnitDoesNotExist"),
                                               ("unit" , string unitName) ]
export
ToJSON ActionResult where
  toJSON (ResEvent e) =
    object [ ("tag", string "Event"), ("event", toJSON e) ]
  toJSON (ResError x) =
    object [ ("tag", string "Error"), ("error", toJSON x)]
  toJSON (ResQuery x) =
    object [ ("tag", string "Query"), ("result", toJSON x)]

partial
export
FromJSON ActionResult where
  fromJSON = withObject "ActionResult" $ \ o => do
     tag <- field o "tag"
     case the String tag of
        "Event" => ResEvent <$> (field o "event")
        "Error" => ResError <$> (field o "error")
        -- We currently cannot deserialise ResQuery content because it's polymorphic
        -- in the result type it contains and there's no witness packed in the JSON
        -- to reconstruct it.
        -- It should not be a problem in the short term because we are only supposed
        -- to store and deserialise events
        "Query" => fail "Cannot convert query results from JSON"
        _ => fail #"Unexpected tag #{tag}"#

export
ToJSON GameState where
  toJSON (MkGameState turn side stateSegment units) =
    object [ ("tag", string "GameState")
            , ("turn" , toJSON turn)
            , ("side", toJSON side)
            , ("stateSegment", toJSON stateSegment)
            , ("units", toJSON units)
            ]

export
ToJSON Game where
  toJSON (MkGame curState gameMap) =
    object [ ("tag", string "Game")
            , ("curState", toJSON curState)
            , ("gameMap" , toJSON gameMap)
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
makePlayerAction game (JObject [ ("tag", JString "MoveTo"), ("unit", JString unitName), ("to", JArray [ JInteger col, JInteger row]) ] ) with (curSegment game)
  makePlayerAction game (JObject [ ("tag", JString "MoveTo"), ("unit", JString unitName), ("to", JArray [ JInteger col, JInteger row]) ] ) | Move = Cmd <$> makeMoveCommand unitName (cast col) (cast row)
  makePlayerAction game (JObject [ ("tag", JString "MoveTo"), ("unit", JString unitName), ("to", JArray [ JInteger col, JInteger row]) ] ) | other = Left ("Invalid command for segment " ++ show other)
makePlayerAction game (JObject [ ("tag", JString "Attack"), ("units", unitNames), ("hex", JArray [ JInteger col, JInteger row]) ] ) with (curSegment game)
  makePlayerAction game (JObject [ ("tag", JString "Attack"), ("units", unitNames), ("hex", JArray [ JInteger col, JInteger row]) ] ) | Combat NoCombat = Cmd <$> makeAttackWithCommand unitNames (cast col) (cast row)
  makePlayerAction game (JObject [ ("tag", JString "Attack"), ("units", unitNames), ("hex", JArray [ JInteger col, JInteger row]) ] ) | other = Left $ "Invalid command for segment " ++ show other
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
makePlayerAction game (JObject [("tag", JString  "Place"), ("unitName", JString unitName), ("position", JArray [ JInteger col, JInteger row] )]) with (curSegment game)
  makePlayerAction game (JObject [("tag", JString  "Place"), ("unitName", JString unitName), ("position", JArray [ JInteger col, JInteger row] )]) | Setup = makePos (cast col) (cast row) >>=   pure . Cmd . Place unitName
  makePlayerAction game (JObject [("tag", JString  "Place"), ("unitName", JString unitName), ("position", JArray [ JInteger col, JInteger row] )]) | other  = Left ("Invalid command for segment " ++ show other)
makePlayerAction game (JObject (("tag", JString  "GetCurrentSegment") :: _)) = Right $ Qry GetCurrentSegment
makePlayerAction _ sexp = Left $ "Unknown command " ++ show sexp
