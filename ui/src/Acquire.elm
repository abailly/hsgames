module Acquire exposing (main)

{-| Comment
@docs main
-}

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

{-| Main -}
main : Program Never
main =
  App.program
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }

type alias Model = { command : String, strings : List String
                   , games : List GameDescription
                   , numPlayers : Int, numRobots : Int  -- for creating new games
                   }

type Msg = Output String -- from server
         | Input String  -- from user
         | ListGames
         | Join GameId
         | CreateGame
         | SetNumPlayers String
         | SetNumRobots String
         | Submit 

subscriptions model =
  listen "ws://localhost:9090" Output
           
init : (Model, Cmd Msg)
init = (Model "" [] [] 1 5, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Output s  -> case Json.decodeString decodeResult s of
                         Ok (GamesList l) ->
                             ({model | games = l}, Cmd.none)
                         _                ->
                             ({model | command = "", strings = s :: model.strings}, Cmd.none)
        Input s   -> ({model | command = s},Cmd.none)
        SetNumPlayers s  -> case String.toInt s of
                                Ok i -> ({model | numPlayers = i},Cmd.none)
                                _    -> (model,Cmd.none)
        SetNumRobots s   -> case String.toInt s of
                                Ok i -> ({model | numRobots = i},Cmd.none)
                                _    -> (model,Cmd.none)
        ListGames -> (model, sendCommand List)
        Join g    -> (model, sendCommand (JoinGame { playerName = "arnaud", gameId =  g}))
        CreateGame -> (model, send "ws://localhost:9090" "{\"tag\": \"NewGame\", \"numHumans\":1, \"numRobots\": 5}")
                           -- sendCommand (NewGame { numHumans = model.numPlayers, numRobots = model.numRobots}))
        Submit    -> ({model | command = ""}, send "ws://localhost:9090" model.command)

sendCommand : Message -> Cmd Msg
sendCommand m = Debug.log ("sending: "++ toString m) <|
                send "ws://localhost:9090" (Json.encode 0 <| encodeMessage m)
                
view : Model -> Html Msg
view model = div []
             [ gamesList model
             , div [ id "messages" ]
                 [ ol [] <| List.map showMessage model.strings ]
             ]

gamesList : Model -> Html Msg
gamesList model = div [ id "games-list" ]
                  [ button [onClick ListGames] [ text "List Games" ]
                  , createGame model
                  , ul [] (List.map displayGame model.games)
                  ]

createGame : Model -> Html Msg
createGame model = div [ id "create-game" ]
                   [ input [ id "num-players", type' "number", onInput SetNumPlayers, A.min "0", A.max "6", value (toString model.numPlayers) ] [] 
                   , input [ id "num-robots", type' "number", onInput SetNumRobots, A.min "0", A.max "6", value (toString model.numRobots) ] [] 
                   , button [ onClick CreateGame ] [ text "New Game" ]
                   ]

displayGame : GameDescription -> Html Msg
displayGame desc = let isLive = if desc.descLive then "live" else ""
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

