module Messages exposing (..)

import Json.Decode as Json exposing ((:=), andThen)
import Json.Encode as Enc

type Message = List
             | NewGame { numHumans : Int, numRobots : Int }
             | JoinGame { playerName : String, gameId : String }
             | Bye

encodeMessage : Message -> Json.Value
encodeMessage msg =
    case msg of
        List       -> Enc.object [("tag", Enc.string "List"), ("contents",Enc.list [])]
        NewGame n  -> Enc.object [("tag", Enc.string "NewGame")
                                 ,("numHumans",Enc.int n.numHumans)
                                 ,("numRobots",Enc.int n.numRobots)]
        JoinGame j -> Enc.object [("tag", Enc.string "JoinGame")
                                 ,("playerName", Enc.string j.playerName)
                                 ,("gameId", Enc.string j.gameId)
                                 ]
        Bye        -> Enc.object [("tag", Enc.string "Bye"), ("contents",Enc.list [])]

type alias GameId = String
type alias PlayerName = String
    
type CommandResult = PlayerRegistered PlayerName GameId
                   | NewGameStarted GameId
                   | GameStarts GameId
                   | GamesList (List GameDescription)
                   | ErrorMessage String

decodeResult : Json.Decoder CommandResult
decodeResult = ("tag" := Json.string) `andThen` makeResult

makeResult : String -> Json.Decoder CommandResult
makeResult tag =
    case tag of
        "PlayerRegistered" -> ("contents" := Json.tuple2 PlayerRegistered Json.string Json.string)
        "NewGameStarted"   -> ("contents" := Json.map NewGameStarted Json.string)
        "GameStarts"       -> ("contents" := Json.map GameStarts Json.string)
        "GamesList"        -> ("contents" := Json.map GamesList (Json.list decodeGameDescription))
        "ErrorMessage"     -> ("contents" := Json.map ErrorMessage Json.string)
        other              -> Json.fail <| "tag " ++ other ++ " is not a known result"
                              
type alias GameDescription = { gameDescId           : GameId
                             , descNumberOfHumans   : Int
                             , descNumberOfRobots   : Int
                             , descRegisteredHumans : List PlayerName
                             , descLive             : Bool
                             }
              

decodeGameDescription : Json.Decoder GameDescription
decodeGameDescription = Json.object5 GameDescription
                        ("gameDescId" := Json.string)
                        ("descNumberOfHumans" := Json.int)
                        ("descNumberOfRobots" := Json.int)
                        ("descRegisteredHumans" := Json.list Json.string)
                        ("descLive" := Json.bool)
                            
