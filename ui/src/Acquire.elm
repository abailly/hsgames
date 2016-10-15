module Acquire exposing (main)

{-| Comment
@docs main
-}

import Char
import Random
import Platform.Sub as Sub
import String
import Json.Decode as Json
import Json.Encode as Json
import Html.App as App
import WebSocket exposing (..)
import Messages exposing (..)
import Model exposing (..)
import View exposing (view)
import Dict

{-| Main -}
main : Program (String, String)
main =
  App.programWithFlags
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }

subscriptions model =
        Sub.batch [ listen model.wsServerUrl Output ]
           
init : (String , String) -> (Model, Cmd Msg)
init (host,port') =
    let randomClientKey = Random.map String.fromList (Random.list 16 <| Random.map Char.fromCode (Random.int 65 90))
    in ({ strings = [], showMessages =  True, errors = [], domain = host <:> port', wsServerUrl = ""
        , game = Register { player = Player "" Human [] Dict.empty 0 } }
       , Random.generate UseKey randomClientKey)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg,model.game) of
        (Output s,_)    -> handleMessages model s
        (UseKey k,_)    -> let url = "ws://" ++ asString model.domain ++ "/" ++ k
                           in Debug.log url <| ({model | wsServerUrl = url}, Cmd.none)
        (SetName s, Register _)
            -> ({model | game = Register { player = player s}}, Cmd.none)
        (RegisterPlayer, Register r)
            -> case String.trim r.player.playerName of
                   "" -> (model, Cmd.none)
                   _  -> ({model | game = SelectGame { player = r.player, games = [], numPlayers = 1, numRobots = 5 }}
                         , sendCommand model List)
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
        (Play n, PlayGame g)
            -> ({model| game = PlayGame { g | possiblePlays = []}}, sendCommand model (Action { selectedPlay = n }))
        (Join g, SelectGame sg)
            -> (model, sendCommand model (JoinGame { playerName = sg.player.playerName, gameId = g}))
        (CreateGame, SelectGame sg)
            -> (model, sendCommand model (NewGame { numHumans = sg.numPlayers, numRobots = sg.numRobots}))
        (Reset, EndOfGame eg)
            -> ({model
                    | strings = []
                    , game = SelectGame { player = eg.player, games = [], numPlayers = 1, numRobots = 5 } }
               , sendCommand model List)
        (HighlightCell tile, PlayGame g)
            -> ({model| game = PlayGame { g | highlightedCell = Just tile }}, Cmd.none)
        (UnhighlightCell, PlayGame g)
            -> ({model| game = PlayGame { g | highlightedCell = Nothing }}, Cmd.none)
        _   -> (model, Cmd.none)
               
handleMessages : Model -> String -> (Model, Cmd Msg)
handleMessages model s =
    case Json.decodeString decodeMessages s of
        Ok (GamesList l) ->
            gamesList model l
        Ok (NewGameStarted _) ->
            (model, sendCommand model List)
        Ok (PlayerRegistered n gid) ->
            (model, sendCommand model List)
        Ok (GameStarts gid) ->
            gameStarts gid model
        Ok (ErrorMessage m) ->
            ({model | errors = m :: model.errors}, Cmd.none)
        Ok (GameUpdated gs) ->
            gameState gs model
        Ok (Played pl) ->
            played pl model
        Ok (GameEnds g) ->
            gameEnds g model
        _                ->
            ({model | strings = s :: model.strings}, Cmd.none)

gameStarts : GameId -> Model -> (Model,Cmd Msg)
gameStarts gid model =
    case model.game of
        SelectGame {player} -> ({model|game = PlayGame {player = player, gameId = gid, board = Dict.empty
                                                       , possiblePlays = [], highlightedCell = Nothing }}, sendCommand model List)
        _                   -> (model, Cmd.none)

gamesList : Model -> List GameDescription -> (Model, Cmd Msg)
gamesList model listOfGames =
    case model.game of
        SelectGame s -> let newGame = SelectGame {s|games = listOfGames}
                        in ({model| game = newGame }, Cmd.none)
        _            -> (model, Cmd.none)

gameState : GameUpdate -> Model -> (Model, Cmd Msg)
gameState {gsPlayer,gsBoard,gsPlayables} model =
    case model.game of
        PlayGame g -> let newGame = PlayGame { g | board = gsBoard, possiblePlays = gsPlayables, player = gsPlayer}
                      in ({model | game = newGame}, Cmd.none)
        _            -> (model, Cmd.none)

played : PlayerPlay -> Model -> (Model, Cmd Msg)
played {gsBoard,gsPlayed} model =
    case model.game of
        PlayGame g -> let newGame = PlayGame { g | board = gsBoard, possiblePlays = []}
                      in ({model | game = newGame, strings = showOrder gsPlayed :: model.strings }, Cmd.none)
        _            -> (model, Cmd.none)

gameEnds : Game -> Model -> (Model, Cmd Msg)
gameEnds {players, gameBoard} model =
    case model.game of
        PlayGame g -> ({model | game = EndOfGame { player = g.player
                                                 , gameId = g.gameId
                                                 , board = gameBoard, gameResult = players }}, Cmd.none)
        _          -> (model, Cmd.none)

sendCommand : Model -> Request -> Cmd Msg
sendCommand model m = send model.wsServerUrl (Json.encode 0 <| encodeRequest m)
