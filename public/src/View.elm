module View exposing (view)

-- {{{

import Dict
import Html exposing (..)
import Html.Attributes as A exposing (class, href, id, max, min, placeholder, src, type_, value)
import Html.Events exposing (..)
import Json.Decode as Json
import Json.Encode as Json
import Messages exposing (..)
import Model exposing (..)
import String exposing (fromInt)



-- }}}


view : Model -> Html Msg
view model =
    div []
        [ viewTitle model
        , displayErrors model
        , playerInput model
        , viewGamesList model
        , gameBoard model
        , viewGameResult model
        , messages model
        ]



-- {{{


viewTitle : Model -> Html Msg
viewTitle model =
    div [ id "game-title" ]
        [ h1 [] [ text "Acquire" ]
        , case model.game of
            Register { player } ->
                h2 [] [ text player.playerName ]

            SelectGame { player } ->
                h2 [] [ text player.playerName ]

            PlayGame { player, gameId } ->
                h2 [] [ text player.playerName, text "@", text gameId ]

            EndOfGame { player, gameId } ->
                h2 [] [ text player.playerName, text "@", text gameId ]
        ]


messages : Model -> Html Msg
messages model =
    let
        toggle =
            if model.showMessages then
                span [ class "fa fa-toggle-down", onClick <| ShowMessages False ] []

            else
                span [ class "fa fa-toggle-right", onClick <| ShowMessages True ] []

        doDisplayList =
            if model.showMessages then
                [ A.style "height" "10em" ]

            else
                [ A.style "height" "0" ]
    in
    div [ id "messages" ]
        [ div [ id "messages-header" ]
            [ h1 [] [ text "Messages" ]
            , toggle
            ]
        , div ([ id "messages-content" ] ++ doDisplayList)
            [ ul [] <| List.map showMessage model.strings
            ]
        ]


displayErrors : Model -> Html Msg
displayErrors model =
    div [ id "errors" ]
        (List.map displayError model.errors)


displayError : String -> Html Msg
displayError error =
    div [ class "error" ]
        [ text error ]


playerInput : Model -> Html Msg
playerInput model =
    case model.game of
        Register p ->
            div [ id "player-id" ]
                [ span [] [ text "Player Name" ]
                , input [ id "player-name", value p.player.playerName, onInput SetName ] []
                , button [ onClick RegisterPlayer ] [ text "Register" ]
                ]

        _ ->
            text ""


gameBoard : Model -> Html Msg
gameBoard model =
    case model.game of
        PlayGame g ->
            div [ id "game-board" ]
                [ div [ class "player" ]
                    [ h1 [] [ text "Player's Hand" ]
                    , span [ class "cash" ] [ text <| fromInt g.player.ownedCash ]
                    , div [ class "stock" ] <| List.map displayStock (Dict.toList g.player.ownedStock)
                    ]
                , div [ class "plays" ]
                    (h1 [] [ text "Possible Plays" ] :: List.indexedMap displayPlay g.possiblePlays)
                , div [ class "board" ]
                    (h1 [] [ text "Current Board" ] :: List.map (displayCell g.highlightedCell) (Dict.toList g.board))
                ]

        _ ->
            text ""


displayStock : ( ChainName, Int ) -> Html Msg
displayStock ( cn, num ) =
    span [ class cn ]
        [ span [ class "stock-count" ] [ text <| fromInt num ]
        ]


displayPlay : Int -> Messages.Order -> Html Msg
displayPlay n order =
    case order of
        Place _ ( r, c ) ->
            span
                [ class "cell empty"
                , onClick <| Play (n + 1)
                , onMouseEnter (HighlightCell ( r, c ))
                , onMouseLeave UnhighlightCell
                ]
                [ span [ class "cell-content" ]
                    [ span [] [ text <| String.fromChar r ++ "-" ++ fromInt c ] ]
                ]

        BuyStock _ cn ->
            span [ class <| "cell chain " ++ cn, onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-usd" ] [] ]
                ]

        SellStock _ cn num price ->
            span [ class <| "cell chain " ++ cn, onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-usd" ] [] ]
                ]

        ExchangeStock _ cf ct count ->
            span [ class <| "cell exchange", onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class <| "buyee-" ++ cf ] []
                    , span [ class <| "buyer-" ++ ct ] []
                    , span [ class "fa fa-lg fa-exchange" ] []
                    ]
                ]

        Fund _ cn _ ->
            span [ class <| "cell chain " ++ cn, onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-building-o" ] [] ]
                ]

        Merge _ _ cf ct ->
            span [ class <| "cell merge", onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class <| "buyee-" ++ cf ] []
                    , span [ class <| "buyer-" ++ ct ] []
                    , span [ class "fa fa-lg fa-building-o" ] []
                    ]
                ]

        Pass ->
            span [ class <| "cell", onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-refresh" ] [] ]
                ]

        EndGame ->
            span [ class <| "cell", onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-stop" ] [] ]
                ]

        Cancel ->
            span [ class <| "cell", onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-backward" ] [] ]
                ]


displayCell : Maybe Tile -> ( Tile, Cell ) -> Html Msg
displayCell highlighted ( ( r, c ) as tile, cell ) =
    let
        hlClass =
            maybe " empty"
                (\hlTile ->
                    if hlTile == tile then
                        " highlighted"

                    else
                        " empty"
                )
                highlighted
    in
    case cell.cellContent of
        Empty ->
            span [ class <| "cell" ++ hlClass ]
                [ span [ class "cell-content" ]
                    [ span [] [ text <| String.fromChar r ++ "-" ++ fromInt c ] ]
                ]

        Neutral _ ->
            span [ class <| "cell neutral" ]
                [ span [ class "cell-content" ]
                    [ span [] [ text <| String.fromChar r ++ "-" ++ fromInt c ] ]
                ]

        Chain n ->
            span [ class <| "cell chain " ++ n ]
                [ span [ class "cell-content" ]
                    [ span [] [ text <| String.fromChar r ++ "-" ++ fromInt c ] ]
                ]

        _ ->
            text ""


viewGamesList : Model -> Html Msg
viewGamesList model =
    case model.game of
        SelectGame sg ->
            div [ id "games-list" ]
                [ createGame model
                , ul [] (List.map displayPossibleGames sg.games)
                ]

        _ ->
            text ""


createGame : Model -> Html Msg
createGame model =
    case model.game of
        SelectGame sg ->
            div [ id "create-game" ]
                [ input [ id "num-players", type_ "number", onInput SetNumPlayers, A.min "0", A.max "6", value (fromInt sg.numPlayers) ] []
                , input [ id "num-robots", type_ "number", onInput SetNumRobots, A.min "0", A.max "6", value (fromInt sg.numRobots) ] []
                , button [ onClick CreateGame ] [ text "New Game" ]
                ]

        _ ->
            text ""


displayPossibleGames : GameDescription -> Html Msg
displayPossibleGames desc =
    let
        isLive =
            if desc.descLive then
                "live"

            else
                ""
    in
    li []
        [ div [ class <| "game-description " ++ isLive ]
            [ span [ class "game-id" ] [ text desc.gameDescId ]
            , span [ class "numPlayers humans" ] [ text <| fromInt desc.descNumberOfHumans ]
            , span [ class "numPlayers robots" ] [ text <| fromInt desc.descNumberOfRobots ]
            , ul [ class "players-list" ]
                (List.map displayPlayer desc.descRegisteredHumans)
            , if desc.descLive then
                text ""

              else
                button [ onClick <| Join desc.gameDescId ] [ text "Join" ]
            ]
        ]


displayPlayer : PlayerName -> Html Msg
displayPlayer p =
    li [] [ text p ]


viewGameResult : Model -> Html Msg
viewGameResult model =
    case model.game of
        EndOfGame g ->
            displayPlayerResults (Dict.values g.gameResult)

        _ ->
            div [ id "game-result-background", A.style "display" "none" ] []


displayPlayerResults : List Player -> Html Msg
displayPlayerResults players =
    let
        winners =
            List.sortBy (\p -> p.ownedCash) players
    in
    div [ id "game-result-background" ]
        [ div [ id "game-result" ]
            (h1 [] [ text "Players' Score" ]
                :: button [ onClick Reset ] [ text "Reset" ]
                :: List.map displayPlayerResult winners
            )
        ]


displayPlayerResult : Player -> Html Msg
displayPlayerResult player =
    div [ class <| "player " ++ Messages.showPlayerType player.playerType ]
        [ span [ class "name" ] [ text player.playerName ]
        , span [ class "cash" ] [ text <| fromInt player.ownedCash ]
        ]



-- * Utilities


onEnter : msg -> Attribute msg
onEnter msg =
    on "keydown" (Json.map (always msg) (Json.map is13 keyCode))


is13 : Int -> Result String ()
is13 code =
    if code == 13 then
        Ok ()

    else
        Err "not the right key code"


showMessage : String -> Html Msg
showMessage s =
    li [ class "message" ] [ text s ]



-- | Optional mapping of some `Maybe a` value
-- This is from Haskell's Data.Maybe package. Given a `Maybe a`, either returns
-- some `defaultValue` if `optional` is `Nothing` or applies given `transform` function
-- on the content of `Just a` value.


maybe : b -> (a -> b) -> Maybe a -> b
maybe defaultVal transform optional =
    Maybe.withDefault defaultVal <|
        Maybe.map transform optional



-- }}}
