module Model exposing (..)

import Messages exposing (..)

type alias Model = { strings : List String, showMessages: Bool
                   , errors : List String
                   , wsServerUrl : String
                   , game : GameState
                   }
    
type GameState = Register   { player : Player }
               | SelectGame { player : Player, games : List GameDescription, numPlayers : Int, numRobots : Int }
               | PlayGame   { player : Player, gameId : GameId, board : GameBoard
                            , possiblePlays : List Messages.Order, highlightedCell : Maybe Tile }
               | EndOfGame  { player : Player, gameId : GameId, board : GameBoard, gameResult : Players }

type Msg = Output String
         | UseKey String
         | SetName String
         | RegisterPlayer
         | ListGames
         | Join GameId
         | CreateGame
         | Play Int
         | SetNumPlayers String
         | SetNumRobots String
         | ShowMessages Bool
         | HighlightCell Tile | UnhighlightCell
         | Reset
