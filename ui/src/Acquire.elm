module Acquire exposing (main)

{-| Comment
@docs main
-}

import Platform.Sub as Sub
import Debug
import String
import Json.Decode as Json
import Json.Encode as Json
import Html exposing (..)
import Html.Attributes exposing (href, src, placeholder, min, max, value, id, class, type')
import Html.Attributes as A
import Html.Events exposing (..)
import Html.App as App
import WebSocket exposing (..)
import Messages exposing (..)
import Dict

{-| Main -}
main : Program String
main =
  App.programWithFlags
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }

type alias Key = String
    
type alias Model = { command : String, strings : List String, showMessages: Bool
                   , games : List GameDescription
                   , numPlayers : Int, numRobots : Int
                   , board : GameBoard, possiblePlays : List Messages.Order, player : Player
                   , errors : List String
                   , gameResult : Maybe Players
                   , clientKey : Key
                   }

type Msg = Output String
         | SetName String
         | ListGames
         | Join GameId
         | CreateGame
         | Play Int
         | SetNumPlayers String
         | SetNumRobots String
         | ShowMessages Bool
         | Reset

subscriptions model =
  Sub.batch [ listen ("ws://localhost:9090/" ++ model.clientKey) Output ]
           
init : Key -> (Model, Cmd Msg)
init key = (Model "" [] True [] 1 5 Dict.empty [] (player "") [] Nothing key, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Output s        -> handleServerMessages model s
        SetName s       -> ({model | player = player s},Cmd.none)
        ShowMessages b  -> ({model | showMessages = b },Cmd.none)
        SetNumPlayers s -> case String.toInt s of
                               Ok i -> ({model | numPlayers = i},Cmd.none)
                               _    -> (model,Cmd.none)
        SetNumRobots s  -> case String.toInt s of
                               Ok i -> ({model | numRobots = i},Cmd.none)
                               _    -> (model,Cmd.none)
        ListGames       -> (model, sendCommand model List)
        Play n          -> ({model| possiblePlays = []}, sendCommand model (Action { selectedPlay = n }))
        Join g          -> (model, sendCommand model (JoinGame { playerName = model.player.playerName, gameId =  g}))
        CreateGame      -> (model, sendCommand model (NewGame { numHumans = model.numPlayers, numRobots = model.numRobots}))
        Reset           -> ({model
                                | command = "", strings = []
                                , board = Dict.empty, possiblePlays = [], player = player (model.player.playerName)
                                , errors = []
                                , gameResult = Nothing}, Cmd.none)

handleServerMessages : Model -> String -> (Model, Cmd Msg)
handleServerMessages model s =
    case Json.decodeString decodeServerMessages s of
        Ok (R (GamesList l)) ->
            ({model | games = l}, Cmd.none)
        Ok (R (NewGameStarted _)) ->
            (model, sendCommand model List)
        Ok (R (PlayerRegistered n gid)) ->
            (model, sendCommand model List)
        Ok (R (GameStarts gid)) ->
            (model, sendCommand model List)
        Ok (R (ErrorMessage m)) ->
            ({model | errors = m :: model.errors}, Cmd.none)
        Ok (M (GameState gs)) ->
            ({model | board = gs.gsBoard, possiblePlays = gs.gsPlayables, player = gs.gsPlayer}, Cmd.none)
        Ok (M (Played pl)) ->
            ({model | board = pl.gsBoard, possiblePlays = [], strings = showOrder pl.gsPlayed :: model.strings }, Cmd.none)
        Ok (M (GameEnds g)) ->
            ({model | board = g.gsEndGame.gameBoard
             , possiblePlays = []
             , gameResult = Just g.gsEndGame.players }, Cmd.none)
        _                ->
            ({model | command = "", strings = s :: model.strings}, Cmd.none)
                
sendCommand : Model -> Message -> Cmd Msg
sendCommand model m = send ("ws://localhost:9090/" ++ model.clientKey) (Json.encode 0 <| encodeMessage m)
                
view : Model -> Html Msg
view model = div []
             [ displayErrors model
             , playerInput model
             , gamesList model
             , gameBoard model
             , gameResult model
             , messages model
             ]

messages: Model -> Html Msg
messages model =
    let toggle = if model.showMessages
                 then span [ class "fa fa-toggle-down", onClick <| ShowMessages False ] []
                 else span [ class "fa fa-toggle-right", onClick <| ShowMessages True ] []
        doDisplayList = if   model.showMessages
                       then []
                       else [ A.style [("display", "none")]]
    in div [ id "messages" ]
        [ div [ id "messages-header" ]
                    [ h1 []  [text "Messages" ]
                    , toggle 
                    ]
        , div ([ id "messages-content"] ++ doDisplayList)
            [ ul []  <| List.map showMessage model.strings
            ]
        ]
                     
displayErrors : Model -> Html Msg
displayErrors model = div [ id "errors" ]
                      (List.map displayError model.errors)
displayError : String -> Html Msg
displayError error = div [ class "error" ]
                     [ text error ]
                         
playerInput : Model -> Html Msg
playerInput model = div [ id "player-id" ]
                    [ span [] [text "Player Name" ]
                    , input [ id "player-name", value model.player.playerName, onInput SetName ] []
                    ]

gameBoard : Model -> Html Msg
gameBoard model = div [ id "game-board" ]
                  [ div [ class "player" ]
                    [ h1 [] [ text "Player's Hand" ]
                    , span [ class "cash" ] [ text <| toString model.player.ownedCash ] 
                    , div [ class "stock" ] <| List.map displayStock (Dict.toList model.player.ownedStock)
                    ]
                  , div [ class "plays" ]
                    (h1 [] [ text "Possible Plays" ] :: (List.indexedMap displayPlay model.possiblePlays))
                  , div [ class "board" ]
                      (h1 [] [ text "Current Board" ] :: List.map displayCell (Dict.toList model.board))
                  ]

displayStock : (ChainName, Int) -> Html Msg
displayStock (cn, num) = span [ class cn ] [
                          span [ class "stock-count" ] [text <| toString num ]
                         ]
    
displayPlay : Int -> Messages.Order -> Html Msg
displayPlay n order =
    case order of
        Place _ (r,c) -> span [ class "cell empty", onClick <| Play (n + 1) ] [
                          span [ class "cell-content"]
                              [ span [] [ text <| String.fromChar r ++ "-" ++ toString c ]]
                         ]
        BuyStock _ cn -> span [ class <| "cell chain " ++ cn, onClick <| Play (n + 1) ] [
                          span [ class "cell-content"]
                              [ span [class "fa fa-lg fa-usd"] []]
                         ]
        SellStock _ cn num price -> span [ class <| "cell chain " ++ cn, onClick <| Play (n + 1) ] [
                                     span [ class "cell-content"]
                                         [ span [class "fa fa-lg fa-exchange"] []]
                                    ]
        ExchangeStock _ cf ct count -> span [ class <| "cell exchange", onClick <| Play (n + 1) ] [
                                     span [ class "cell-content"]
                                         [ span [ class <| "buyee-" ++ cf ] []
                                         , span [ class <| "buyer-" ++ ct ] []
                                         , span [class "fa fa-lg fa-exchange"] []]
                                       ]
        Fund _ cn _   -> span [ class <| "cell chain " ++ cn, onClick <| Play (n + 1) ] [
                          span [ class "cell-content"]
                              [ span [class "fa fa-lg fa-building-o"] []]
                         ]
        Merge _ _ cf ct  -> span [ class <| "cell merge", onClick <| Play (n + 1) ] [
                             span [ class "cell-content"]
                                 [ span [ class <| "buyee-" ++ cf ] []
                                 , span [ class <| "buyer-" ++ ct ] []
                                 , span [class "fa fa-lg fa-building-o"] []]
                            ]
        Pass          -> span [ class <| "cell", onClick <| Play (n + 1) ] [
                          span [ class "cell-content"]
                              [ span [class "fa fa-lg fa-refresh"] []]
                         ]
        EndGame       -> span [ class <| "cell", onClick <| Play (n + 1) ] [
                          span [ class "cell-content"]
                              [ span [class "fa fa-lg fa-stop"] []]
                         ]
        Cancel       -> span [ class <| "cell", onClick <| Play (n + 1) ] [
                          span [ class "cell-content"]
                              [ span [class "fa fa-lg fa-backward"] []]
                         ]
                             
displayCell : (Tile,Cell) -> Html Msg
displayCell ((r,c), cell) =
    case cell.cellContent of
        Empty   -> span [ class "cell empty" ] [
                    span [ class "cell-content"]
                        [ span [] [ text <| String.fromChar r ++ "-" ++ toString c ]]
                   ]
        Neutral _ -> span [ class "cell neutral" ] [
                    span [ class "cell-content"]
                        [ span [] [ text <| String.fromChar r ++ "-" ++ toString c ]]
                   ]
        Chain n -> span [ class <| "cell chain " ++ n ] [
                    span [ class "cell-content"]
                        [ span [] [ text <| String.fromChar r ++ "-" ++ toString c ]]
                   ]
        _       -> text "" -- TODO
                  
                        
                      
gamesList : Model -> Html Msg
gamesList model = div [ id "games-list" ]
                  [ button [onClick ListGames] [ text "List Games" ]
                  , createGame model
                  , ul [] (List.map displayPossibleGames model.games)
                  ]

createGame : Model -> Html Msg
createGame model = div [ id "create-game" ]
                   [ input [ id "num-players", type' "number", onInput SetNumPlayers, A.min "0", A.max "6", value (toString model.numPlayers) ] [] 
                   , input [ id "num-robots", type' "number", onInput SetNumRobots, A.min "0", A.max "6", value (toString model.numRobots) ] [] 
                   , button [ onClick CreateGame ] [ text "New Game" ]
                   ]

displayPossibleGames : GameDescription -> Html Msg
displayPossibleGames desc =
    let isLive = if desc.descLive then "live" else ""
    in li []
        [ div [ class <| "game-description " ++ isLive ]
              [ span [ class "game-id" ] [ text desc.gameDescId ]
              , span [ class "numPlayers humans" ] [ text <| toString desc.descNumberOfHumans ]
              , span [ class "numPlayers robots" ] [ text <| toString desc.descNumberOfRobots ]
              , ul [ class "players-list" ]
                  (List.map displayPlayer desc.descRegisteredHumans)
              , if   desc.descLive
                then text ""
                else button [ onClick <| Join desc.gameDescId ] [ text "Join" ]
              ]
        ]
                       
displayPlayer : PlayerName -> Html Msg
displayPlayer p = li [] [ text p ]

gameResult : Model -> Html Msg
gameResult model =
    case model.gameResult of
        Nothing -> div [ id "game-result-background", A.style [("display", "none")]] []
        Just g  -> displayPlayerResults (Dict.values g)

displayPlayerResults : List Player -> Html Msg
displayPlayerResults players =
    let winners = List.sortBy (\ p -> p.ownedCash) players
    in  div [ id "game-result-background" ]
        [ div [ id "game-result" ]
              (h1 [] [ text "Players' Score" ] ::
                   button [ onClick Reset ] [ text "Reset" ] :: List.map displayPlayerResult winners)
        
        ]

displayPlayerResult : Player -> Html Msg
displayPlayerResult player =
    div [ class <| "player " ++ Messages.showPlayerType player.playerType ]
    [ span [ class "name" ] [ text player.playerName ]
    , span [ class "cash" ] [ text <| toString player.ownedCash ]
    ]
    
onEnter : msg -> Attribute msg
onEnter msg =
  on "keydown" (Json.map (always msg) (Json.customDecoder keyCode is13))

is13 : Int -> Result String ()
is13 code =
  if code == 13 then
    Ok ()

  else
    Err "not the right key code"

showMessage : String -> Html Msg
showMessage s = li [ class "message" ] [ text s ]

