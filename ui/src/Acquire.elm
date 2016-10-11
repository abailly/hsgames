module Acquire exposing (main)

{-| Comment
@docs main
-}

import Char
import Random
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
main : Program Never
main =
  App.program
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }

type alias Model = { strings : List String, showMessages: Bool
                   , errors : List String
                   , wsServerUrl : String
                   , game : GameState
                   }
    
type GameState = Register { player : Player }
                 | SelectGame { player : Player, games : List GameDescription, numPlayers : Int, numRobots : Int }
                 | PlayGame { player : Player, board : GameBoard, possiblePlays : List Messages.Order }
                 | EndOfGame { player : Player, board : GameBoard, gameResult : Maybe Players }

type Msg = Output String
         | UseKey String
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
    Debug.log "subscribing to WS "<|
        Sub.batch [ listen model.wsServerUrl Output ]
           
init : (Model, Cmd Msg)
init =
    let randomClientKey = Random.map String.fromList (Random.list 16 <| Random.map Char.fromCode (Random.int 65 90))
    in ({ strings = [], showMessages =  True, errors = [], wsServerUrl = ""
        , player = player "" }
       , Random.generate UseKey randomClientKey)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg,model.game) of
        (Output s,_)    -> handleServerMessages model s
        (UseKey k,_)    -> ({model | wsServerUrl = "ws://localhost:9090/" ++ k}, Cmd.none)
        (SetName s, Register _)
            -> ({model | game = Register { player = player s}},Cmd.none)
        (ShowMessages b,_) -> ({model | showMessages = b },Cmd.none)
        (SetNumPlayers s,SelectGame sg)
            -> case String.toInt s of
                   Ok i -> ({model | game = SelectGame {sg|numPlayers = i}},Cmd.none)
                   _    -> (model,Cmd.none)
        (SetNumRobots s,SelectGame sg)
            -> case String.toInt s of
                   Ok i -> ({model | game = SelectGame {sg|numRobots = i}},Cmd.none)
                   _    -> (model,Cmd.none)
        (ListGames, SelectGame sg)
            -> (model, sendCommand model List)
        Play n          -> ({model| possiblePlays = []}, sendCommand model (Action { selectedPlay = n }))
        Join g          -> (model, sendCommand model (JoinGame { playerName = model.player.playerName, gameId =  g}))
        CreateGame      -> (model, sendCommand model (NewGame { numHumans = model.numPlayers, numRobots = model.numRobots}))
        Reset           -> ({model
                                | strings = []
                                , board = Dict.empty, possiblePlays = [], player = player (model.player.playerName)
                                , errors = []
                                , gameResult = Nothing}, Cmd.none)

handleServerMessages : Model -> String -> (Model, Cmd Msg)
handleServerMessages model s =
    case Json.decodeString decodeServerMessages s of
        Ok (R (GamesList l)) ->
            gamesList model l
        Ok (R (NewGameStarted _)) ->
            (model, sendCommand model List)
        Ok (R (PlayerRegistered n gid)) ->
            (model, sendCommand model List)
        Ok (R (GameStarts gid)) ->
            gameStarts gid model
        Ok (R (ErrorMessage m)) ->
            ({model | errors = m :: model.errors}, Cmd.none)
        Ok (M (GameState gs)) ->
            gameState gs model
        Ok (M (Played pl)) ->
            played pl model
        Ok (M (GameEnds g)) ->
            gameEnds g model
        _                ->
            ({model | strings = s :: model.strings}, Cmd.none)

gameStarts : GameId -> Model -> (Model,Cmd Msg)
gameStarts gid model =
    case model.game of
        SelectGame {player} -> ({model|game = PlayGame {player = player, board = Dict.empty, possiblePlays = [] }}, sendCommand model List)
        _                   -> (model, Cmd.none)

gamesList : Model -> List GameDescription -> (Model, Cmd Msg)
gamesList model listOfGames =
    case model.game of
        SelectGame s -> let newGame = SelectGame {s|games = listOfGames}
                        in ({model| game = newGame }, Cmd.none)
        _            -> (model, Cmd.none)

gameState : { gsPlayer : Player
            , gsBoard : GameBoard
            , gsPlayables : List Messages.Order
            } -> Model -> (Model, Cmd Msg)
gameState {gsPlayer,gsBoard,gsPlayables} model =
    case model.game of
        PlayGame g -> let newGame = PlayGame { g | board = gsBoard, possiblePlays = gsPlayables, player = gsPlayer}
                      in ({model | game = newGame}, Cmd.none)
        _            -> (model, Cmd.none)

played {gsBoard,gsPlayed} model =
    case model.game of
        PlayGame g -> let newGame = PlayGame { g | board = gsBoard, possiblePlays = []}
                      in ({model | game = newGame, strings = showOrder gsPlayed :: model.strings }, Cmd.none)
        _            -> (model, Cmd.none)

gameEnds {gsEndGame} model =
    case model.game of
        PlayGame p -> ({model | game = EndOfGame { player = p.player
                                                 , board = gsEndGame.gameBoard, gameResult = Just gsEndGame.players }}, Cmd.none)
        _            -> (model, Cmd.none)

sendCommand : Model -> Message -> Cmd Msg
sendCommand model m = send model.wsServerUrl (Json.encode 0 <| encodeMessage m)
                
view : Model -> Html Msg
view model = div []
             [ displayErrors model
             , playerInput model
             , viewGamesList model
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
                       then [A.style [("height", "10em")]]
                       else [ A.style [("height", "0")]]
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
                  
                        
                      
viewGamesList : Model -> Html Msg
viewGamesList model = div [ id "games-list" ]
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

