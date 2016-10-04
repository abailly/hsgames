module Messages exposing (..)

import Json.Decode as Json exposing ((:=), andThen)
import Json.Encode as Enc
import Dict
import String

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

-- Results & Messages from Server

type ServerMessages = R CommandResult
                    | M PlayMessage

decodeServerMessages : Json.Decoder ServerMessages
decodeServerMessages = Json.oneOf [ Json.map R decodeResult
                                  , Json.map M decodePlayMessage
                                  ]

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
                            

-- Game Play
type PlayMessage = GameState { gsPlayer : Player
                             , gsBoard : GameBoard
                             , gsPlayables : List Order
                             }
                 | Played    { gsPlayerName : PlayerName
                             , gsBoard :  GameBoard
                             , gsPlayed : Order
                             }
                 | GameEnds  { gsEndGame : Game }

decodePlayMessage : Json.Decoder PlayMessage
decodePlayMessage = ("tag" := Json.string) `andThen` makePlayMessage

makePlayMessage : String -> Json.Decoder PlayMessage
makePlayMessage tag =
    case tag of 
        "GameState" -> Json.object3 (\ p b t -> GameState { gsPlayer = p, gsBoard = b, gsPlayables = t })
                       ("gsPlayer" := decodePlayer)
                       ("gsBoard" := decodeBoard)
                       ("gsPlayables" := Json.list decodeOrder)
        other       -> Json.fail <| other ++ " is not a known play message"
            
type alias Game = { gameId       : GameId
                  , gameBoard    : GameBoard
                  , players      : Players
                  , hotelChains  : HotelChains
                  }

-- Stock
type alias Stock = Dict.Dict ChainName Int

decodeStock : Json.Decoder Stock
decodeStock = Json.map Dict.fromList <|
              Json.list              <|
              Json.tuple2 (,) Json.string Json.int
-- Player

type PlayerType = Human | Robot

decodeType : Json.Decoder PlayerType
decodeType = Json.string `andThen` \ s -> case s of
                                              "Human" -> Json.succeed Human
                                              "Robot" -> Json.succeed Robot
                                              other   -> Json.fail <| other ++ " is not a valid player type"
                                                         
type alias Players = Dict.Dict PlayerName Player

type alias Player = { playerName : PlayerName
                    , playerType : PlayerType
                    , tiles      : List Tile
                    , ownedStock : Stock
                    , ownedCash  : Int
                    } 

decodePlayer : Json.Decoder Player
decodePlayer = Json.object5 Player
               ("playerName" := Json.string)
               ("playerType" := decodeType)
               ("tiles"      := Json.list decodeTile)
               ("ownedStock" := decodeStock)
               ("ownedCash"  := Json.int)
               
-- Board
type alias Tile = (Char,Int)

decodeTile : Json.Decoder Tile
decodeTile = Json.object2 (,)
             ("row" := Json.string `andThen` firstChar)
             ("col" := Json.int)

firstChar : String -> Json.Decoder Char
firstChar s = case String.uncons s of
                  Nothing    -> Json.fail "no char found"
                  Just (c,_) -> Json.succeed c
                                
type alias GameBoard = Dict.Dict Tile Cell

decodeBoard : Json.Decoder GameBoard
decodeBoard = Json.map Dict.fromList
              (Json.list <| Json.tuple2 (,) decodeTile decodeCell) 
              
type alias Cell = { cellCoord   : Tile
                  , cellContent : Content
                  }

decodeCell : Json.Decoder Cell
decodeCell = Json.object2 Cell
             ("cellCoord" := decodeTile)
             ("cellContent" := decodeContent)
             
type Content = Empty
             | Playable  -- ^Used for highlighting purpose
             | Neutral Tile
             | Chain ChainName

decodeContent : Json.Decoder Content
decodeContent = ("tag" := Json.string) `andThen` makeContent

makeContent : String -> Json.Decoder Content
makeContent tag =
    case tag of
        "Empty"    -> Json.succeed Empty
        "Playable" -> Json.succeed Playable 
        "Neutral"  -> "contents" := Json.map Neutral decodeTile
        "Chain"    -> "contents" := Json.map Chain Json.string
        other      -> Json.fail <| other ++ " is not a valid content"

-- Hotels

type alias ChainName = String

type alias HotelChain = { chainName  : ChainName
                        , chainTiles : List Tile
                        , chainStock : Int
                        } 


type alias HotelChains = Dict.Dict ChainName HotelChain

-- Orders
type MergerPhase = TakeOver Tile (List ChainName)
                 | DisposeStock { initialPlayer   : PlayerName
                                , buyerChain      : ChainName
                                , buyeeChain      : ChainName
                                , buyeePrice      : Int
                                , playersToDecide : List PlayerName
                                }

type Phase = PlaceTile
           | FundChain Tile
           | BuySomeStock Int
           | ResolveMerger MergerPhase Turn
           | GameEnding

type alias Turn = (PlayerName, Phase)

type Order = Place PlayerName Tile
           | Merge PlayerName Tile ChainName ChainName
           | Fund PlayerName ChainName Tile
           | BuyStock PlayerName ChainName
           | SellStock PlayerName ChainName Int Int
           | ExchangeStock PlayerName ChainName ChainName Int
           | Pass
           | EndGame
           | Cancel

decodeOrder : Json.Decoder Order
decodeOrder =  ("tag" := Json.string) `andThen` makeOrder

makeOrder : String -> Json.Decoder Order
makeOrder tag =
    case tag of
        "Place"         -> "contents" := Json.tuple2 Place Json.string decodeTile
        "Merge"         -> "contents" := Json.tuple4 Merge Json.string decodeTile Json.string Json.string
        "Fund"          -> "contents" := Json.tuple3 Fund Json.string Json.string decodeTile
        "BuyStock"      -> "contents" := Json.tuple2 BuyStock Json.string Json.string
        "SellStock"     -> "contents" := Json.tuple4 SellStock Json.string Json.string Json.int Json.int
        "ExchangeStock" -> "contents" := Json.tuple4 ExchangeStock Json.string Json.string Json.string Json.int
        "Pass"          -> Json.succeed Pass
        "EndGame"       -> Json.succeed EndGame
        "Cancel"        -> Json.succeed Cancel
        other           -> Json.fail <| other ++ " is not a valid order"
