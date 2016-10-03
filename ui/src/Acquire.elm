module Acquire exposing (main)

{-| Comment
@docs main
-}

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (href, src, placeholder, value, id, class)
import Html.Events exposing (..)
import Html.App as App
import WebSocket exposing (..)

{-| Main -}
main : Program Never
main =
  App.program
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }

type alias Model = { command : String, strings : List String }

type Msg = Output String -- from server
         | Input String  -- from user
         | Submit 

subscriptions model =
  listen "ws://localhost:9090" Output
           
init : (Model, Cmd Msg)
init = (Model "" [], Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Output s -> ({command = "", strings = s :: model.strings}, Cmd.none)
        Input s  -> ({model | command = s},Cmd.none)
        Submit   -> ({model | command = ""}, send "ws://localhost:9090" model.command)

view : Model -> Html Msg
view model = div [ id "messages" ]
             [ label [ ] [
                    input [ id "commands", onInput Input, onEnter Submit , value model.command ] [ ]
                   ]
             , div [ id "server" ] <|
                 List.map showMessage model.strings
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
showMessage s = div [ class "message" ] [ text s ]

