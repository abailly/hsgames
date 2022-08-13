port module Game exposing
    ( Messages(..)
    , Model
    , Msg(..)
    , Play(..)
    , Player(..)
    , Segment(..)
    , Side(..)
    , decodeMessages
    , decodePos
    , divStyle
    , init
    , isNothing
    , main
    , update
    , view
    , viewDiv
    )

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html5.DragDrop as DragDrop exposing (Position)
import Json.Decode as Json exposing (andThen, field, index, succeed)
import Json.Encode as Enc
import List exposing (foldr)
import Random
import String exposing (fromInt)
import Tuple exposing (first)



-- JavaScript usage: app.ports.websocketIn.send(response);


port websocketIn : (String -> msg) -> Sub msg



-- JavaScript usage: app.ports.websocketOut.subscribe(handler);


port wsOut : String -> Cmd msg


type alias Pos =
    ( Int, Int )


encodePos : Pos -> Json.Value
encodePos ( x, y ) =
    Enc.list Enc.int [ y, x ]


type alias UnitName =
    String


type Army
    = Russian
    | German


armyToString : Army -> String
armyToString army =
    case army of
        Russian ->
            "russian"

        German ->
            "german"


type alias Unit =
    { name : UnitName
    , img : String
    , army : Army
    , position : Pos
    }


type alias Game =
    { units : Dict.Dict UnitName Unit
    , positions : Dict.Dict Pos (List Unit)
    , dragDrop : DragDrop.Model UnitName Pos
    , gameId : String
    , gameSegment : GameSegment
    }


type Side
    = Axis
    | Allies


encodeSide : Side -> Json.Value
encodeSide =
    toString >> Enc.string


decodeSide : Json.Decoder Side
decodeSide =
    let
        mkSide s =
            case s of
                "Axis" ->
                    succeed Axis

                "Allies" ->
                    succeed Allies

                other ->
                    Json.fail ("Uknown side: " ++ other)
    in
    andThen mkSide Json.string


toString : Side -> String
toString side =
    case side of
        Axis ->
            "Axis"

        Allies ->
            "Allies"


fromString : String -> Side
fromString s =
    case s of
        "Axis" ->
            Axis

        _ ->
            Allies


type alias PreGame =
    { games : List GameDescription
    , gameId : String
    , playerKey : String
    , side : Maybe Side
    }


type Player
    = Human String
    | Robot
    | NoPlayer


type alias GameDescription =
    { gameId : String, axisPlayer : Player, alliesPlayer : Player, segment : GameSegment }


type Model
    = NoGame PreGame
    | Waiting { gameId : String, mySide : Side }
    | InGame Game
    | InCombat Game CombatState


type Action
    = GetCurrentSegment
    | Place UnitName Pos
    | Next
    | MoveTo UnitName Pos
    | Attack (List UnitName) Pos


encodeAction : Action -> Json.Value
encodeAction act =
    case act of
        GetCurrentSegment ->
            Enc.object
                [ ( "tag", Enc.string "GetCurrentSegment" )
                ]

        Place u p ->
            Enc.object
                [ ( "tag", Enc.string "Place" )
                , ( "unitName", Enc.string u )
                , ( "position", encodePos p )
                ]

        MoveTo u p ->
            Enc.object
                [ ( "tag", Enc.string "MoveTo" )
                , ( "unit", Enc.string u )
                , ( "to", encodePos p )
                ]

        Attack us p ->
            Enc.object
                [ ( "tag", Enc.string "Attack" )
                , ( "units", Enc.list Enc.string us )
                , ( "hex", encodePos p )
                ]

        Next ->
            Enc.object
                [ ( "tag", Enc.string "Next" ) ]


type Request
    = {- Request current positions of all units -} PositionsQ
    | JoinGame { gameId : String, side : Side }
    | Connect { playerKey : String }
    | NewGame { gameId : String }
    | StartGame { gameId : String }
    | Action { gameId : String, action : Action }
    | ListGames


encodeRequest : Request -> Json.Value
encodeRequest msg =
    case msg of
        PositionsQ ->
            Enc.list Enc.string [ "positions?" ]

        Connect j ->
            Enc.object
                [ ( "tag", Enc.string "Connect" )
                , ( "playerKey", Enc.string j.playerKey )
                ]

        NewGame g ->
            Enc.object
                [ ( "tag", Enc.string "NewGame" )
                , ( "gameId", Enc.string g.gameId )
                ]

        StartGame g ->
            Enc.object
                [ ( "tag", Enc.string "StartGame" )
                , ( "gameId", Enc.string g.gameId )
                ]

        ListGames ->
            Enc.object
                [ ( "tag", Enc.string "ListGames" ) ]

        JoinGame j ->
            Enc.object
                [ ( "tag", Enc.string "JoinGame" )
                , ( "gameId", Enc.string j.gameId )
                , ( "side", encodeSide j.side )
                ]

        Action a ->
            Enc.object
                [ ( "tag", Enc.string "Action" )
                , ( "gameId", Enc.string a.gameId )
                , ( "action", encodeAction a.action )
                ]


type Msg
    = DragDropMsg (DragDrop.Msg UnitName Pos)
    | StartNewGame
    | Join String Side
    | Input String
    | NewGameWithId String
    | SetSide Side
    | NextSegment


germanOOB : List ( String, String )
germanOOB =
    [ ( "17-hq-recto", "17-hq-recto" )
    , ( "17-kg1-recto", "17-kg1-recto" )
    , ( "17-kg1-verso", "17-kg1-verso" )
    , ( "17-kg2-recto", "17-kg2-recto" )
    , ( "17-kg2-verso", "17-kg2-verso" )
    , ( "112/20Pz", "20pz-112-recto" )
    , ( "112/20Pz", "20pz-112-verso" )
    , ( "21/20Pz", "20pz-21-recto" )
    , ( "21/20Pz", "20pz-21-verso" )
    , ( "59/20Pz", "20pz-59-recto" )
    , ( "59/20Pz", "20pz-59-verso" )
    , ( "HQ/20Pz", "20pz-hq-recto" )
    , ( "464-hq-recto", "464-hq-recto" )
    , ( "464-kg1-recto", "464-kg1-recto" )
    , ( "464-kg1-verso", "464-kg1-verso" )
    , ( "464-kg2-recto", "464-kg2-recto" )
    , ( "464-kg2-verso", "464-kg2-verso" )
    , ( "546-kg1-recto", "546-kg1-recto" )
    , ( "546-kg1-verso", "546-kg1-verso" )
    , ( "546-kg2-recto", "546-kg2-recto" )
    , ( "546-kg2-verso", "546-kg2-verso" )
    , ( "546vg-hq-recto", "546vg-hq-recto" )
    , ( "57pzk-hq-recto", "57pzk-hq-recto" )
    , ( "615-687-recto", "615-687-recto" )
    , ( "615-687-verso", "615-687-verso" )
    , ( "615-hq-recto", "615-hq-recto" )
    , ( "615-kg-recto", "615-kg-recto" )
    , ( "732-arty-recto", "732-arty-recto" )
    , ( "777-arty-recto", "777-arty-recto" )
    , ( "black-cross", "black-cross" )
    , ( "brgpzg-1-recto", "brgpzg-1-recto" )
    , ( "brgpzg-1-verso", "brgpzg-1-verso" )
    , ( "brgpzg-2-recto", "brgpzg-2-recto" )
    , ( "brgpzg-2-verso", "brgpzg-2-verso" )
    , ( "brgpzg-hq-recto", "brgpzg-hq-recto" )
    , ( "brgpzg-pz-recto", "brgpzg-pz-recto" )
    , ( "brgpzg-pz-verso", "brgpzg-pz-verso" )
    , ( "gdpzk-hq-recto", "gdpzk-hq-recto" )
    , ( "hg1-hq-recto", "hg1-hq-recto" )
    , ( "hg1-pz1-recto", "hg1-pz1-recto" )
    , ( "hg1-pz1-verso", "hg1-pz1-verso" )
    , ( "hg1-pzg1-recto", "hg1-pzg1-recto" )
    , ( "hg1-pzg1-verso", "hg1-pzg1-verso" )
    , ( "hg1-pzg2-recto", "hg1-pzg2-recto" )
    , ( "hg1-pzg2-verso", "hg1-pzg2-verso" )
    , ( "hg2-hq-recto", "hg2-hq-recto" )
    , ( "hg2-pzg3-recto", "hg2-pzg3-recto" )
    , ( "hg2-pzg3-verso", "hg2-pzg3-verso" )
    , ( "hg2-pzg4-recto", "hg2-pzg4-recto" )
    , ( "hg2-pzg4-verso", "hg2-pzg4-verso" )
    , ( "kg72-recto", "kg72-recto" )
    , ( "kg72-verso", "kg72-verso" )
    , ( "truck-recto", "truck-recto" )
    ]


russianOOB : List ( String, String )
russianOOB =
    [ ( "10dp-25-recto", "10dp-25-recto" )
    , ( "10dp-27-recto", "10dp-27-recto" )
    , ( "10dp-29-recto", "10dp-29-recto" )
    , ( "10dp-hq-recto", "10dp-hq-recto" )
    , ( "14gd-36-recto", "14gd-36-recto" )
    , ( "14gd-36-verso", "14gd-36-verso" )
    , ( "14gd-38-recto", "14gd-38-recto" )
    , ( "14gd-38-verso", "14gd-38-verso" )
    , ( "14gd-41-recto", "14gd-41-recto" )
    , ( "14gd-41-verso", "14gd-41-verso" )
    , ( "14gd-hq-recto", "14gd-hq-recto" )
    , ( "14s-arty-recto", "14s-arty-recto" )
    , ( "14s-recto", "14s-recto" )
    , ( "15gd-44-recto", "15gd-44-recto" )
    , ( "15gd-44-verso", "15gd-44-verso" )
    , ( "15gd-47-recto", "15gd-47-recto" )
    , ( "15gd-47-verso", "15gd-47-verso" )
    , ( "15gd-50-recto", "15gd-50-recto" )
    , ( "15gd-50-verso", "15gd-50-verso" )
    , ( "15gd-hq-recto", "15gd-hq-recto" )
    , ( "1kp-recto", "1kp-recto" )
    , ( "1kp-verso", "1kp-verso" )
    , ( "214-776-recto", "214-776-recto" )
    , ( "214-780-recto", "214-780-recto" )
    , ( "214-788-recto", "214-788-recto" )
    , ( "214-hq-recto", "214-hq-recto" )
    , ( "254-929-recto", "254-929-recto" )
    , ( "254-933-recto", "254-933-recto" )
    , ( "254-936-recto", "254-936-recto" )
    , ( "254-hq-recto", "254-hq-recto" )
    , ( "294-857-recto", "294-857-recto" )
    , ( "294-859-recto", "294-859-recto" )
    , ( "294-861-recto", "294-861-recto" )
    , ( "294-hq-recto", "294-hq-recto" )
    , ( "2awp-hq-recto", "2awp-hq-recto" )
    , ( "4gd-12-recto", "4gd-12-recto" )
    , ( "4gd-13-recto", "4gd-13-recto" )
    , ( "4gd-14-recto", "4gd-14-recto" )
    , ( "4gd-29-recto", "4gd-29-recto" )
    , ( "4gd-3-recto", "4gd-3-recto" )
    , ( "4gd-3-verso", "4gd-3-verso" )
    , ( "4gtc-hq-recto", "4gtc-hq-recto" )
    , ( "13/5DP", "5dp-13-recto" )
    , ( "13/5DP", "5dp-13-verso" )
    , ( "15/5DP", "5dp-15-recto" )
    , ( "15/5DP", "5dp-15-verso" )
    , ( "17/5DP", "5dp-17-recto" )
    , ( "17/5DP", "5dp-17-verso" )
    , ( "HQ/5DP", "5dp-hq-recto" )
    , ( "6l-arty-recto", "6l-arty-recto" )
    , ( "7dp-33-recto", "7dp-33-recto" )
    , ( "7dp-33-verso", "7dp-33-verso" )
    , ( "7dp-35-recto", "7dp-35-recto" )
    , ( "7dp-35-verso", "7dp-35-verso" )
    , ( "7dp-37-recto", "7dp-37-recto" )
    , ( "7dp-37-verso", "7dp-37-verso" )
    , ( "7dp-hq-recto", "7dp-hq-recto" )
    , ( "7gd-24-recto", "7gd-24-recto" )
    , ( "7gd-24-verso", "7gd-24-verso" )
    , ( "7gd-25-recto", "7gd-25-recto" )
    , ( "7gd-25-verso", "7gd-25-verso" )
    , ( "7gd-26-recto", "7gd-26-recto" )
    , ( "7gd-26-verso", "7gd-26-verso" )
    , ( "7gd-57-recto", "7gd-57-recto" )
    , ( "7gmc-hq-recto", "7gmc-hq-recto" )
    , ( "7h-arty-recto", "7h-arty-recto" )
    , ( "8c-arty-recto", "8c-arty-recto" )
    , ( "8dp-32-recto", "8dp-32-recto" )
    , ( "8dp-32-verso", "8dp-32-verso" )
    , ( "8dp-34-recto", "8dp-34-recto" )
    , ( "8dp-34-verso", "8dp-34-verso" )
    , ( "8dp-36-recto", "8dp-36-recto" )
    , ( "8dp-36-verso", "8dp-36-verso" )
    , ( "8dp-hq-recto", "8dp-hq-recto" )
    , ( "95gd-284-recto", "95gd-284-recto" )
    , ( "95gd-284-verso", "95gd-284-verso" )
    , ( "95gd-287-recto", "95gd-287-recto" )
    , ( "95gd-287-verso", "95gd-287-verso" )
    , ( "95gd-290-recto", "95gd-290-recto" )
    , ( "95gd-290-verso", "95gd-290-verso" )
    , ( "95gd-hq-recto", "95gd-hq-recto" )
    , ( "9dp-26-recto", "9dp-26-recto" )
    , ( "9dp-26-verso", "9dp-26-verso" )
    , ( "9dp-28-recto", "9dp-28-recto" )
    , ( "9dp-28-verso", "9dp-28-verso" )
    , ( "9dp-30-recto", "9dp-30-recto" )
    , ( "9dp-30-verso", "9dp-30-verso" )
    , ( "9dp-hq-recto", "9dp-hq-recto" )
    , ( "9s-arty-recto", "9s-arty-recto" )
    , ( "air-support-recto", "air-support-recto" )
    , ( "air-support-verso", "air-support-verso" )
    , ( "red-star", "red-star" )
    , ( "truck-recto", "truck-recto" )
    ]


russianUnits : List Unit
russianUnits =
    List.map (\( n, img ) -> { name = n, img = img, army = Russian, position = ( 100, 100 ) }) russianOOB


germanUnits : List Unit
germanUnits =
    List.map (\( n, img ) -> { name = n, img = img, army = German, position = ( 200, 200 ) }) germanOOB


init : { port_ : String, host : String, gameId : String, playerKey : String } -> ( Model, Cmd Msg )
init i =
    let
        model =
            NoGame { games = [], gameId = i.gameId, playerKey = i.playerKey, side = Nothing }
    in
    ( model
    , sendCommand model <| Connect { playerKey = i.playerKey }
    )


inGame : String -> Model
inGame gameId =
    InGame
        { units =
            Dict.fromList <| List.map (\u -> ( u.name, u )) <| russianUnits ++ germanUnits
        , positions =
            Dict.fromList [ ( ( 100, 100 ), russianUnits ), ( ( 200, 200 ), germanUnits ) ]
        , dragDrop = DragDrop.init
        , gameId = gameId
        , gameSegment = { turn = 0, side = Axis, segment = Setup }
        }



-- | The current turn of the game.


type alias Turn =
    Int


showTurn : Turn -> String
showTurn turn =
    fromInt turn


type Segment
    = Setup
    | Supply
    | Move
    | Combat CombatPhase


showSegment : Segment -> String
showSegment s =
    case s of
        Setup ->
            "Setup"

        Supply ->
            "Supply"

        Move ->
            "Move"

        Combat _ ->
            "Combat"


type CombatPhase
    = NoCombat
    | AssignTacticalSupport CombatSideAndState
    | AssignStrategicSupport CombatSideAndState
    | Resolve CombatState
    | ApplyLosses CombatSideAndState


type alias CombatSideAndState =
    { side : Side, combat : CombatState }


type alias CombatState =
    { combatHex : Pos
    , attackers : EngagedUnits
    , defenders : EngagedUnits
    , losses : Maybe Losses
    }


type alias EngagedUnits =
    { base : List Unit
    , tacticalSupport : List Unit
    , strategicSupport : Int
    }


type alias Losses =
    { attackerLoss : Int, defenderLoss : Int }



--   GameEnd


type alias GameSegment =
    { turn : Turn, side : Side, segment : Segment }


type Play
    = Placed String Pos
    | AlliesSetupDone
    | SegmentChanged Segment Segment
    | Moved String Pos Pos Int
    | CombatEngaged (List UnitName) (List UnitName) Pos


type Messages
    = {- Server acknowledged player's connection -} Connected
    | GamesList (List GameDescription)
    | NewGameCreated String
    | PlayerJoined String Side
    | GameStarted String
    | CurrentGameSegment GameSegment
    | PlayerPlayed String Play
    | Positions (List ( Unit, Pos ))
    | PlayerReJoined String (List Messages)


tuple2 : (a -> b -> c) -> Json.Decoder a -> Json.Decoder b -> Json.Decoder c
tuple2 ctor decodea decodeb =
    Json.map2 ctor (Json.index 0 decodea) (Json.index 1 decodeb)


tuple3 : (a -> b -> c -> d) -> Json.Decoder a -> Json.Decoder b -> Json.Decoder c -> Json.Decoder d
tuple3 ctor decodea decodeb decodec =
    Json.map3 ctor (Json.index 0 decodea) (Json.index 1 decodeb) (Json.index 2 decodec)


decodeMessages : Json.Decoder Messages
decodeMessages =
    field "tag" Json.string |> andThen makeMessages


decodeUnit : Json.Decoder Unit
decodeUnit =
    tuple3 (\name army position -> { name = name, img = name, army = army, position = position })
        (field "name" Json.string)
        decodeArmy
        decodePos


decodeArmy : Json.Decoder Army
decodeArmy =
    let
        mkArmy s =
            case s of
                "German" ->
                    succeed German

                "Russian" ->
                    succeed Russian

                "Polish" ->
                    succeed Russian

                other ->
                    Json.fail ("Uknown army type " ++ other)
    in
    andThen mkArmy Json.string


decodePos : Json.Decoder Pos
decodePos =
    Json.map2 (\a b -> ( a, b ))
        (index 1 Json.int)
        (index 0 Json.int)


decodeTurn : Json.Decoder Turn
decodeTurn =
    let
        mkTurn i =
            if i >= 0 && i <= 5 then
                succeed (6 - i)

            else
                Json.fail ("Invalid turn value: " ++ fromInt i)
    in
    andThen mkTurn Json.int


decodeSegment : Json.Decoder Segment
decodeSegment =
    let
        mkSegment s =
            case s of
                "Setup" ->
                    succeed Setup

                "Supply" ->
                    succeed Supply

                "Move" ->
                    succeed Move

                "Combat" ->
                    Json.map Combat (field "phase" decodeCombatPhase)

                other ->
                    Json.fail ("Uknown Segment type " ++ other)
    in
    andThen mkSegment <| field "tag" Json.string


decodeCombatPhase : Json.Decoder CombatPhase
decodeCombatPhase =
    let
        mkCombatPhase s =
            case s of
                "NoCombat" ->
                    Json.succeed NoCombat

                "AssignStrategicSupport" ->
                    Json.map AssignStrategicSupport decodeSideAndState

                "AssignTacticalSupport" ->
                    Json.map AssignTacticalSupport decodeSideAndState

                "Resolve" ->
                    Json.map Resolve decodeCombatState

                "ApplyLosses" ->
                    Json.map ApplyLosses decodeSideAndState

                _ ->
                    Json.fail <| "Unknown combat phase " ++ s
    in
    andThen mkCombatPhase <| field "tag" Json.string


decodeSideAndState : Json.Decoder CombatSideAndState
decodeSideAndState =
    Json.map2 CombatSideAndState (field "side" decodeSide) (field "combat" decodeCombatState)


decodeCombatState : Json.Decoder CombatState
decodeCombatState =
    Json.map4 CombatState
        (field "combatHex" decodePos)
        (field "attackers" decodeEngagedUnits)
        (field "defenders" decodeEngagedUnits)
        (field "losses" <| Json.maybe decodeLosses)


decodeEngagedUnits : Json.Decoder EngagedUnits
decodeEngagedUnits =
    Json.map3 EngagedUnits
        (field "base" <| Json.list decodeUnitAndPos)
        (field "tacticalSupport" <| Json.list decodeUnitAndPos)
        (field "strategicSupport" Json.int)


decodeLosses : Json.Decoder Losses
decodeLosses =
    Json.map2 Losses
        (index 0 Json.int)
        (index 1 Json.int)


decodeUnitAndPos : Json.Decoder Unit
decodeUnitAndPos =
    tuple2 (\a b -> { a | position = b })
        decodeGameUnit
        decodePos


decodeGameUnit : Json.Decoder Unit
decodeGameUnit =
    Json.map2 (\n a -> { name = n, img = n, army = a, position = ( 0, 0 ) })
        (Json.field "name" Json.string)
        (Json.field "nation" decodeArmy)


decodeGameSegment : Json.Decoder GameSegment
decodeGameSegment =
    Json.map3 GameSegment
        (field "turn" decodeTurn)
        (field "side" decodeSide)
        (field "segment" decodeSegment)


makeMessages : String -> Json.Decoder Messages
makeMessages tag =
    case tag of
        "Positions" ->
            field "positions" (Json.map Positions (Json.list decodeUnitPos))

        "Connected" ->
            Json.succeed Connected

        "PlayerReJoined" ->
            Json.map2 PlayerReJoined (field "gameId" Json.string) (field "events" <| Json.list decodeMessages)

        "GamesList" ->
            Json.map GamesList (field "games" <| Json.list decodeSingleGame)

        "NewGameCreated" ->
            Json.map NewGameCreated (field "gameId" Json.string)

        "PlayerJoined" ->
            Json.map2 PlayerJoined (field "gameId" Json.string) (field "side" decodeSide)

        "GameStarted" ->
            Json.map GameStarted (field "gameId" Json.string)

        "CurrentGameSegment" ->
            Json.map CurrentGameSegment <| decodeGameSegment

        "PlayerPlayed" ->
            Json.map2 PlayerPlayed
                (field "gameId" Json.string)
                (field "result" decodePlay)

        other ->
            Json.fail ("Unknown tag " ++ other)


decodeSingleGame : Json.Decoder GameDescription
decodeSingleGame =
    Json.map4 GameDescription
        (field "gameId" Json.string)
        (field "axisPlayer" decodePlayer)
        (field "alliesPlayer" decodePlayer)
        (field "segment" decodeGameSegment)


decodePlayer : Json.Decoder Player
decodePlayer =
    field "tag" Json.string
        |> andThen
            (\tag ->
                case tag of
                    "HumanPlayer" ->
                        Json.map Human (field "playerKey" Json.string)

                    "RobotPlayer" ->
                        succeed Robot

                    "NoPlayer" ->
                        succeed NoPlayer

                    _ ->
                        Json.fail ("Unknown player type " ++ tag)
            )


decodePlay : Json.Decoder Play
decodePlay =
    let
        decodePlayBody : String -> Json.Decoder Play
        decodePlayBody tag =
            case tag of
                "Placed" ->
                    Json.map2 Placed
                        (field "unit" Json.string)
                        (field "pos" decodePos)

                "Moved" ->
                    Json.map4 Moved
                        (field "unit" Json.string)
                        (field "from" decodePos)
                        (field "to" decodePos)
                        (field "cost" Json.int)

                "CombatEngaged" ->
                    Json.map3 CombatEngaged
                        (field "attackers" <| Json.list Json.string)
                        (field "defenders" <| Json.list Json.string)
                        (field "target" decodePos)

                "AlliesSetupDone" ->
                    Json.succeed AlliesSetupDone

                "SegmentChanged" ->
                    Json.map2 SegmentChanged
                        (field "from" decodeSegment)
                        (field "to" decodeSegment)

                other ->
                    Json.fail <| "Unknown play tag: " ++ other
    in
    field "tag" Json.string
        |> andThen
            decodePlayBody


decodeUnitPos : Json.Decoder ( Unit, Pos )
decodeUnitPos =
    tuple2 (\a b -> ( a, b )) decodeUnit decodePos


handleMessages : Model -> String -> ( Model, Cmd Msg )
handleMessages model s =
    Json.decodeString decodeMessages s
        |> (\msg ->
                case msg of
                    Err _ ->
                        ( model, Cmd.none )

                    Ok m ->
                        updateModel m model
           )


updateModel : Messages -> Model -> ( Model, Cmd Msg )
updateModel msg model =
    case msg of
        Connected ->
            ( model, sendCommand model ListGames )

        GamesList games ->
            case model of
                NoGame p ->
                    ( NoGame { p | games = games }, Cmd.none )

                _ ->
                    ( Debug.log "model: " model, Cmd.none )

        NewGameCreated gid ->
            case model of
                NoGame p ->
                    case p.side of
                        Just side ->
                            ( model, sendCommand model (JoinGame { gameId = gid, side = side }) )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayerJoined gid sd ->
            case model of
                NoGame p ->
                    case p.side of
                        Just side ->
                            ( Waiting { gameId = gid, mySide = side }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Waiting g ->
                    if g.mySide /= sd then
                        ( model, sendCommand model (StartGame { gameId = g.gameId }) )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayerReJoined gid events ->
            case model of
                InGame _ ->
                    ( model, Cmd.none )

                _ ->
                    ( List.foldl (\mg mod -> first (updateModel mg mod)) model (GameStarted gid :: List.reverse events)
                    , sendCommand model (Action { gameId = gid, action = GetCurrentSegment })
                    )

        GameStarted gid ->
            case model of
                InGame _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        newModel =
                            inGame gid
                    in
                    ( newModel, sendCommand model (Action { gameId = gid, action = GetCurrentSegment }) )

        Positions _ ->
            ( model, Cmd.none )

        CurrentGameSegment gameStage ->
            let
                gs =
                    Debug.log "Handling game segment" gameStage
            in
            case model of
                InGame g ->
                    ( InGame { g | gameSegment = gs }, Cmd.none )

                InCombat g c ->
                    ( InCombat { g | gameSegment = gs } c, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayerPlayed _ (Placed unitName position) ->
            case model of
                InGame game ->
                    ( InGame <| setUnitPositionTo unitName position game, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayerPlayed _ (Moved unitName _ to _) ->
            case model of
                InGame game ->
                    ( InGame <| setUnitPositionTo unitName to game, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayerPlayed _ (CombatEngaged atks defs pos) ->
            case model of
                InGame game ->
                    let
                        combatState =
                            startCombat game atks defs pos
                    in
                    ( InCombat game combatState, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayerPlayed _ (SegmentChanged _ _) ->
            case model of
                InGame game ->
                    ( model, sendCommand model (Action { gameId = game.gameId, action = GetCurrentSegment }) )

                _ ->
                    ( model, Cmd.none )

        PlayerPlayed _ AlliesSetupDone ->
            case model of
                InGame game ->
                    ( model, sendCommand model (Action { gameId = game.gameId, action = GetCurrentSegment }) )

                _ ->
                    ( model, Cmd.none )


startCombat : Game -> List UnitName -> List UnitName -> Pos -> CombatState
startCombat game atks defs combatHex =
    { combatHex = combatHex
    , attackers = { base = List.filterMap (findUnitPosition game) atks, tacticalSupport = [], strategicSupport = 0 }
    , defenders = { base = List.filterMap (findUnitPosition game) defs, tacticalSupport = [], strategicSupport = 0 }
    , losses = Nothing
    }


findUnitPosition : Game -> UnitName -> Maybe Unit
findUnitPosition game unitName =
    Dict.get unitName game.units


setUnitPositionTo : String -> Pos -> Game -> Game
setUnitPositionTo unitName position game =
    let
        setPos pos u =
            Maybe.map (\unit -> { unit | position = pos }) u
    in
    { game
        | units =
            Dict.update (Debug.log "unit name" unitName) (setPos position) game.units
        , positions =
            updatePositions (Dict.get unitName game.units) position game.positions
    }


updatePositions : Maybe Unit -> Pos -> Dict.Dict Pos (List Unit) -> Dict.Dict Pos (List Unit)
updatePositions maybeUnit newPosition positions =
    case maybeUnit of
        Nothing ->
            positions

        Just unit ->
            let
                remove =
                    Maybe.map (\us -> List.filter (\u -> u.name /= unit.name) us)

                add units =
                    let
                        newPos =
                            { unit | position = newPosition }
                    in
                    case units of
                        Nothing ->
                            Just [ newPos ]

                        Just us ->
                            Just (newPos :: us)
            in
            Dict.update newPosition add <|
                Dict.update unit.position
                    remove
                    (Debug.log "updating positions" positions)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NoGame p ->
            case msg of
                StartNewGame ->
                    let
                        randomGameId =
                            Random.map String.fromList (Random.list 8 <| Random.map Char.fromCode (Random.int 65 90))
                    in
                    ( model, Random.generate NewGameWithId randomGameId )

                NewGameWithId gid ->
                    ( model, sendCommand model (NewGame { gameId = gid }) )

                Input s ->
                    handleMessages model s

                DragDropMsg _ ->
                    ( model, Cmd.none )

                SetSide side ->
                    ( NoGame { p | side = Just side }, Cmd.none )

                NextSegment ->
                    ( model, Cmd.none )

                Join gameId side ->
                    ( model, sendCommand model (JoinGame { gameId = gameId, side = side }) )

        Waiting _ ->
            case msg of
                Input s ->
                    handleMessages model s

                DragDropMsg _ ->
                    ( model, Cmd.none )

                StartNewGame ->
                    ( model, Cmd.none )

                NewGameWithId _ ->
                    ( model, Cmd.none )

                SetSide _ ->
                    ( model, Cmd.none )

                NextSegment ->
                    ( model, Cmd.none )

                Join _ _ ->
                    ( model, Cmd.none )

        InGame game ->
            case msg of
                Input s ->
                    handleMessages model s

                DragDropMsg msg_ ->
                    let
                        ( model_, result ) =
                            DragDrop.update msg_ game.dragDrop

                        newModel =
                            InGame
                                { game
                                    | dragDrop = model_
                                }
                    in
                    case result of
                        Just ( unitName, position, _ ) ->
                            ( newModel
                            , case game.gameSegment.segment of
                                Setup ->
                                    sendCommand newModel (Action { gameId = game.gameId, action = Place unitName position })

                                Move ->
                                    sendCommand newModel (Action { gameId = game.gameId, action = MoveTo unitName position })

                                Combat NoCombat ->
                                    sendCommand newModel (Action { gameId = game.gameId, action = Attack [ unitName ] position })

                                _ ->
                                    Cmd.none
                            )

                        Nothing ->
                            ( newModel, Cmd.none )

                NextSegment ->
                    ( model, sendCommand model (Action { gameId = game.gameId, action = Next }) )

                StartNewGame ->
                    ( model, Cmd.none )

                NewGameWithId _ ->
                    ( model, Cmd.none )

                SetSide _ ->
                    ( model, Cmd.none )

                Join _ _ ->
                    ( model, Cmd.none )

        InCombat game combat ->
            ( model, Cmd.none )


divStyle : Pos -> List (Attribute Msg)
divStyle ( x, y ) =
    let
        sine =
            sin (pi / 3)

        ( top, left ) =
            ( if modBy 2 y == 0 then
                x * 133

              else
                x * 133 + 66
            , truncate <| toFloat y * 133 * sine
            )
    in
    [ class "hex"
    , style "top" (String.fromInt top ++ "px")
    , style "left" (String.fromInt left ++ "px")
    ]


parkingStyle : Army -> List (Attribute msg)
parkingStyle army =
    case army of
        Russian ->
            [ class "russian" ]

        German ->
            [ class "german" ]


view : Model -> Html Msg
view model =
    case model of
        NoGame p ->
            viewNoGame p

        InGame game ->
            viewInGame game

        InCombat game _ ->
            viewInGame game

        Waiting w ->
            viewWaiting w


viewWaiting : { gameId : String, mySide : Side } -> Html Msg
viewWaiting waiting =
    div []
        [ label [] [ text <| "Waiting for " ++ toString (otherSide waiting.mySide) ++ " player to join" ]
        ]


otherSide : Side -> Side
otherSide side =
    case side of
        Axis ->
            Allies

        Allies ->
            Axis


viewNoGame : PreGame -> Html Msg
viewNoGame p =
    let
        viewNewGame =
            case p.side of
                Nothing ->
                    []

                _ ->
                    [ button [ onClick StartNewGame ] [ text "New Game" ] ]

        viewPlayer player =
            case player of
                Human pk ->
                    text pk

                Robot ->
                    text "Robot"

                NoPlayer ->
                    text "-"

        actionsForPlayer gameId player side =
            case player of
                NoPlayer ->
                    [ button [ onClick <| Join gameId side ] [ text <| "Join " ++ toString side ] ]

                Human pk ->
                    if pk == p.playerKey then
                        [ button [ onClick <| Join gameId side ] [ text <| "Join " ++ toString side ] ]

                    else
                        []

                Robot ->
                    []

        actions description =
            actionsForPlayer description.gameId description.axisPlayer Axis ++ actionsForPlayer description.gameId description.alliesPlayer Allies

        viewGameState description =
            div
                [ class "segment" ]
                ([ label [] [ text "Turn: " ]
                 , span [] [ text <| showTurn description.segment.turn ]
                 , label [] [ text "Side: " ]
                 , span [] [ text <| toString description.segment.side ]
                 , label [] [ text "Segment: " ]
                 , span [] [ text <| showSegment description.segment.segment ]
                 ]
                    ++ actions description
                )

        listGame description =
            li [ class "game-description" ]
                [ label [] [ text "Game Id: " ]
                , span [] [ text description.gameId ]
                , label [] [ text "Axis: " ]
                , span [] [ viewPlayer description.axisPlayer ]
                , label [] [ text "Allies: " ]
                , span [] [ viewPlayer description.alliesPlayer ]
                , label [] [ text "Segment: " ]
                , viewGameState description
                ]

        listGames =
            List.map listGame p.games
    in
    div []
        ([ div [ class "games-list" ]
            [ ul [] listGames ]
         , div [ class "new-game" ]
            [ label [] [ text "Player: " ]
            , span [] [ text p.playerKey ]
            ]
         , div []
            [ label [] [ text "Side: " ]
            , select [ onInput (fromString >> SetSide) ]
                (List.map
                    sideToOption
                    [ Allies, Axis ]
                )
            ]
         ]
            ++ viewNewGame
        )


sideToOption : Side -> Html Msg
sideToOption side =
    option [ value (toString side) ] [ text (toString side) ]


viewInGame : Game -> Html Msg
viewInGame game =
    div []
        [ viewSegment game.gameSegment
        , viewMap game
        ]


viewSegment : GameSegment -> Html Msg
viewSegment { turn, side, segment } =
    div
        [ class "segment" ]
        [ label [] [ text "Turn: " ]
        , span [] [ text <| showTurn turn ]
        , label [] [ text "Side: " ]
        , span [] [ text <| toString side ]
        , label [] [ text "Segment: " ]
        , span [] [ text <| showSegment segment ]
        , button [ onClick NextSegment ] [ text "Next" ]
        ]


viewMap : Game -> Html Msg
viewMap game =
    let
        dropId =
            DragDrop.getDropId game.dragDrop

        droppablePosition =
            DragDrop.getDroppablePosition game.dragDrop

        pos j =
            List.map (\i -> ( j, i )) <| List.range 0 22

        positions =
            div [ class "positions" ] <| List.map (\p -> viewDiv (divStyle p) game.positions dropId droppablePosition p) <| List.concat (List.map pos <| List.range 0 12)

        russianParking =
            viewParking (parkingStyle Russian) game.positions dropId droppablePosition ( 100, 100 )

        germanParking =
            viewParking (parkingStyle German) game.positions dropId droppablePosition ( 200, 200 )
    in
    div [ class "map" ]
        [ positions, russianParking, germanParking ]


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Just _ ->
            False

        Nothing ->
            True


highlighted : Maybe c -> Maybe Position -> c -> List (Attribute msg)
highlighted dropId droppablePosition position =
    if dropId |> Maybe.map ((==) position) |> Maybe.withDefault False then
        case droppablePosition of
            Nothing ->
                []

            Just pos ->
                if pos.y < pos.height // 2 then
                    [ style "background-color" "cyan" ]

                else
                    [ style "background-color" "magenta" ]

    else
        []


viewParking : List (Attribute Msg) -> Dict.Dict Pos (List Unit) -> Maybe Pos -> Maybe Position -> Pos -> Html Msg
viewParking style_ positions dropId droppablePosition position =
    let
        highlight =
            highlighted dropId droppablePosition position

        units =
            Maybe.withDefault [] <| Dict.get position positions

        styledFloating =
            always
                [ style "margin" "5px" ]
    in
    div
        (style_
            ++ highlight
            ++ DragDrop.droppable DragDropMsg position
        )
        (viewUnits units styledFloating)


viewDiv : List (Attribute Msg) -> Dict.Dict Pos (List Unit) -> Maybe Pos -> Maybe Position -> Pos -> Html Msg
viewDiv style_ positions dropId droppablePosition position =
    let
        highlight =
            highlighted dropId droppablePosition position

        units =
            Maybe.withDefault [] <| Dict.get position positions

        styleStacked index =
            let
                shift =
                    index * 20
            in
            [ style "margin" "5px"
            , style "top" (fromInt shift ++ "px")
            , style "left" (fromInt shift ++ "px")
            , style "z-index" (fromInt shift)
            ]
    in
    div
        (style_
            ++ highlight
            ++ DragDrop.droppable DragDropMsg position
        )
        (viewUnits units styleStacked)


viewUnits : List Unit -> (number -> List (Attribute Msg)) -> List (Html Msg)
viewUnits units styling =
    let
        mkImg unit ( otherUnits, index ) =
            ( img
                (src ("assets/" ++ armyToString unit.army ++ "/" ++ unit.img ++ ".png")
                    :: DragDrop.draggable DragDropMsg unit.name
                    ++ styling index
                )
                []
                :: otherUnits
            , index + 1
            )
    in
    List.foldl mkImg ( [], 0 ) units |> first


main : Program { port_ : String, host : String, gameId : String, playerKey : String } Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


sendCommand : Model -> Request -> Cmd Msg
sendCommand _ r =
    wsOut
        (Enc.encode 0 <| encodeRequest r)


subscriptions : Model -> Sub Msg
subscriptions _ =
    websocketIn Input
