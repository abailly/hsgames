module Messages exposing (..)

import Dict
import Json.Decode as Json exposing (andThen, field)
import Json.Encode as Enc
import String exposing (fromChar, fromInt)



-- | Requests from client to server


type Request
    = List
    | NewGame { numHumans : Int, numRobots : Int }
    | JoinGame { playerName : String, gameId : String }
    | Action { selectedPlay : Int }
    | Bye


encodeRequest : Request -> Json.Value
encodeRequest msg =
    case msg of
        List ->
            Enc.object [ ( "tag", Enc.string "ListGames" ), ( "contents", Enc.list Enc.int [] ) ]

        Action n ->
            Enc.object
                [ ( "tag", Enc.string "Action" )
                , ( "selectedPlay", Enc.int n.selectedPlay )
                ]

        NewGame n ->
            Enc.object
                [ ( "tag", Enc.string "NewGame" )
                , ( "numHumans", Enc.int n.numHumans )
                , ( "numRobots", Enc.int n.numRobots )
                ]

        JoinGame j ->
            Enc.object
                [ ( "tag", Enc.string "JoinGame" )
                , ( "playerName", Enc.string j.playerName )
                , ( "gameId", Enc.string j.gameId )
                ]

        Bye ->
            Enc.object [ ( "tag", Enc.string "Bye" ), ( "contents", Enc.list Enc.int [] ) ]


type alias GameId =
    String


type alias PlayerName =
    String



-- Results & Messages from Server


type Messages
    = PlayerRegistered PlayerName GameId
    | NewGameStarted GameId
    | GameStarts GameId
    | GamesList (List GameDescription)
    | ErrorMessage String
    | GameUpdated GameUpdate
    | Played PlayerPlay
    | GameEnds Game


type alias GameUpdate =
    { gsPlayer : Player
    , gsBoard : GameBoard
    , gsPlayables : List Order
    }


type alias PlayerPlay =
    { gsPlayerName : PlayerName
    , gsBoard : GameBoard
    , gsPlayed : Order
    }


decodeMessages : Json.Decoder Messages
decodeMessages =
    andThen makeMessages (field "tag" Json.string)


makeMessages : String -> Json.Decoder Messages
makeMessages tag =
    case tag of
        "PlayerRegistered" ->
            field "contents" (tuple2 PlayerRegistered Json.string Json.string)

        "NewGameStarted" ->
            field "contents" (Json.map NewGameStarted Json.string)

        "GameStarts" ->
            field "contents" (Json.map GameStarts Json.string)

        "GamesList" ->
            field "contents" (Json.map GamesList (Json.list decodeGameDescription))

        "ErrorMessage" ->
            field "contents" (Json.map ErrorMessage Json.string)

        "GameState" ->
            Json.map GameUpdated <|
                Json.map3 GameUpdate
                    (field "gsPlayer" decodePlayer)
                    (field "gsBoard" decodeBoard)
                    (field "gsPlayables" <| Json.list decodeOrder)

        "Played" ->
            Json.map Played <|
                Json.map3 PlayerPlay
                    (field "gsPlayerName" Json.string)
                    (field "gsBoard" decodeBoard)
                    (field "gsPlayed" decodeOrder)

        "GameEnds" ->
            Json.map GameEnds
                (field "gsEndGame" decodeGame)

        other ->
            Json.fail <| "tag " ++ other ++ " is not a known server message tag"


type alias GameDescription =
    { gameDescId : GameId
    , descNumberOfHumans : Int
    , descNumberOfRobots : Int
    , descRegisteredHumans : List PlayerName
    , descLive : Bool
    }


decodeGameDescription : Json.Decoder GameDescription
decodeGameDescription =
    Json.map5 GameDescription
        (field "gameDescId" Json.string)
        (field "descNumberOfHumans" Json.int)
        (field "descNumberOfRobots" Json.int)
        (field "descRegisteredHumans" (Json.list Json.string))
        (field "descLive" Json.bool)



-- Game Play


type alias Game =
    { gameId : GameId
    , gameBoard : GameBoard
    , players : Players
    }


decodeGame : Json.Decoder Game
decodeGame =
    Json.map3 Game
        (field "gameId" Json.string)
        (field "gameBoard" decodeBoard)
        (field "players" decodePlayers)



-- Stock


type alias Stock =
    Dict.Dict ChainName Int


toPair : a -> b -> ( a, b )
toPair a b =
    ( a, b )


decodeStock : Json.Decoder Stock
decodeStock =
    field "stock"
        (Json.map Dict.fromList <|
            Json.list <|
                tuple2 toPair Json.string Json.int
        )



-- Player


type PlayerType
    = Human
    | Robot


showPlayerType : PlayerType -> String
showPlayerType t =
    case t of
        Human ->
            "Human"

        Robot ->
            "Robot"


decodeType : Json.Decoder PlayerType
decodeType =
    andThen
        (\s ->
            case s of
                "Human" ->
                    Json.succeed Human

                "Robot" ->
                    Json.succeed Robot

                other ->
                    Json.fail <| other ++ " is not a valid player type"
        )
        Json.string


type alias Players =
    Dict.Dict PlayerName Player


type alias Player =
    { playerName : PlayerName
    , playerType : PlayerType
    , tiles : List Tile
    , ownedStock : Stock
    , ownedCash : Int
    }


player : String -> Player
player name =
    Player name Human [] Dict.empty 6000


decodePlayer : Json.Decoder Player
decodePlayer =
    Json.map5 Player
        (field "playerName" Json.string)
        (field "playerType" decodeType)
        (field "tiles" (Json.list decodeTile))
        (field "ownedStock" decodeStock)
        (field "ownedCash" Json.int)


decodePlayers : Json.Decoder Players
decodePlayers =
    Json.dict decodePlayer



-- Board


type alias Tile =
    ( Char, Int )


showTile : Tile -> String
showTile tile =
    case tile of
        ( c, i ) ->
            fromChar c ++ fromInt i


decodeTile : Json.Decoder Tile
decodeTile =
    Json.map2 toPair
        (field "row" (andThen firstChar Json.string))
        (field "col" Json.int)


firstChar : String -> Json.Decoder Char
firstChar s =
    case String.uncons s of
        Nothing ->
            Json.fail "no char found"

        Just ( c, _ ) ->
            Json.succeed c


type alias GameBoard =
    Dict.Dict Tile Cell


decodeBoard : Json.Decoder GameBoard
decodeBoard =
    Json.map Dict.fromList
        (Json.list <| Json.map2 toPair (Json.index 0 decodeTile) (Json.index 1 decodeCell))


type alias Cell =
    { cellCoord : Tile
    , cellContent : Content
    }


decodeCell : Json.Decoder Cell
decodeCell =
    Json.map2 Cell
        (field "cellCoord" decodeTile)
        (field "cellContent" decodeContent)


type Content
    = Empty
    | Playable -- ^Used for highlighting purpose
    | Neutral Tile
    | Chain ChainName


decodeContent : Json.Decoder Content
decodeContent =
    andThen makeContent (field "tag" Json.string)


makeContent : String -> Json.Decoder Content
makeContent tag =
    case tag of
        "Empty" ->
            Json.succeed Empty

        "Playable" ->
            Json.succeed Playable

        "Neutral" ->
            field "contents" (Json.map Neutral decodeTile)

        "Chain" ->
            field "contents" (Json.map Chain Json.string)

        other ->
            Json.fail <| other ++ " is not a valid content"



-- Hotels


type alias ChainName =
    String


type alias HotelChain =
    { chainName : ChainName
    , chainTiles : List Tile
    , chainStock : Int
    }


type alias HotelChains =
    Dict.Dict ChainName HotelChain



-- Orders


type MergerPhase
    = TakeOver Tile (List ChainName)
    | DisposeStock
        { initialPlayer : PlayerName
        , buyerChain : ChainName
        , buyeeChain : ChainName
        , buyeePrice : Int
        , playersToDecide : List PlayerName
        }


type Phase
    = PlaceTile
    | FundChain Tile
    | BuySomeStock Int
    | ResolveMerger MergerPhase Turn
    | GameEnding


type alias Turn =
    ( PlayerName, Phase )


type Order
    = Place PlayerName Tile
    | Merge PlayerName Tile ChainName ChainName
    | Fund PlayerName ChainName Tile
    | BuyStock PlayerName ChainName
    | SellStock PlayerName ChainName Int Int
    | ExchangeStock PlayerName ChainName ChainName Int
    | Pass
    | EndGame
    | Cancel


showOrder : Order -> String
showOrder order =
    case order of
        Place n t ->
            n ++ " plays @" ++ showTile t

        Merge pn tile buyee buyer ->
            pn ++ " merges " ++ buyee ++ " into " ++ buyer ++ " @" ++ showTile tile

        Fund pn chainName tile ->
            pn ++ " founds " ++ chainName ++ " @" ++ showTile tile

        BuyStock pn chainName ->
            pn ++ " buy 1 share of " ++ chainName

        SellStock pn chainName shares price ->
            let
                sharesstring =
                    if shares > 1 then
                        " shares "

                    else
                        " share "
            in
            pn ++ " sells " ++ fromInt shares ++ sharesstring ++ " of " ++ chainName ++ " at " ++ fromInt price ++ "$"

        ExchangeStock pn buyee buyer shares ->
            let
                sharesstring =
                    if shares > 1 then
                        " shares "

                    else
                        " share "
            in
            pn
                ++ " exchanges "
                ++ fromInt shares
                ++ sharesstring
                ++ " of "
                ++ buyee
                ++ " against "
                ++ fromInt (shares // 2)
                ++ sharesstring
                ++ " of "
                ++ buyer

        Pass ->
            "pass"

        EndGame ->
            "end game"

        Cancel ->
            "cancel"


decodeOrder : Json.Decoder Order
decodeOrder =
    andThen makeOrder (field "tag" Json.string)


tuple2 : (a -> b -> c) -> Json.Decoder a -> Json.Decoder b -> Json.Decoder c
tuple2 ctor decodea decodeb =
    Json.map2 ctor (Json.index 0 decodea) (Json.index 1 decodeb)


tuple3 : (a -> b -> c -> d) -> Json.Decoder a -> Json.Decoder b -> Json.Decoder c -> Json.Decoder d
tuple3 ctor decodea decodeb decodec =
    Json.map3 ctor (Json.index 0 decodea) (Json.index 1 decodeb) (Json.index 2 decodec)


tuple4 : (a -> b -> c -> d -> e) -> Json.Decoder a -> Json.Decoder b -> Json.Decoder c -> Json.Decoder d -> Json.Decoder e
tuple4 ctor decodea decodeb decodec decoded =
    Json.map4 ctor (Json.index 0 decodea) (Json.index 1 decodeb) (Json.index 2 decodec) (Json.index 3 decoded)


makeOrder : String -> Json.Decoder Order
makeOrder tag =
    case tag of
        "Place" ->
            field "contents" (tuple2 Place Json.string decodeTile)

        "Merge" ->
            field "contents" (tuple4 Merge Json.string decodeTile Json.string Json.string)

        "Fund" ->
            field "contents" (tuple3 Fund Json.string Json.string decodeTile)

        "BuyStock" ->
            field "contents" (tuple2 BuyStock Json.string Json.string)

        "SellStock" ->
            field "contents" (tuple4 SellStock Json.string Json.string Json.int Json.int)

        "ExchangeStock" ->
            field "contents" (tuple4 ExchangeStock Json.string Json.string Json.string Json.int)

        "Pass" ->
            Json.succeed Pass

        "EndGame" ->
            Json.succeed EndGame

        "Cancel" ->
            Json.succeed Cancel

        other ->
            Json.fail <| other ++ " is not a valid order"
