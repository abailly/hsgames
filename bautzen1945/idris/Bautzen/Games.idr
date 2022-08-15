||| Stateful protocol for managing players and games before they actually start
module Bautzen.Games

import Bautzen.Game
import Bautzen.Game.Core
import Bautzen.Id
import Bautzen.REPL.JSON
import Data.SortedMap
import Data.String.Parser
import Data.Vect
import Decidable.Equality
import Language.JSON

import JSON

%default covering

||| Lifecycle protocol for games.
|||
||| The @gameId@ type parameter is used to distinguish commands for
||| each different managed games.
public export
data GameCommand : Type where

  ||| Create a new game with given id.
  NewGame : (newGameId : Id) -> GameCommand

  ||| List all known games this player can join
  ListGames : GameCommand

  ||| A player identified by @playerKey@ joins given game
  JoinGame : (gameId : Id) -> (side : Side) -> GameCommand

  ||| Start a game.
  ||| Game's players must all be set.
  StartGame : (newGameId : Id) -> GameCommand

  ||| Game-specific action
  Action : { gameSegment : GameSegment} -> (gameId : Id) -> PlayerAction gameSegment -> GameCommand

  ||| Given player leaves game
  Bye : (gameId : Id) ->  GameCommand

public  export
data PlayerType =
     ||| Player is a human with given id.
     HumanPlayer Id
     | RobotPlayer
     | NoPlayer

Eq PlayerType where
  HumanPlayer i == HumanPlayer i' = i == i'
  RobotPlayer == RobotPlayer = True
  NoPlayer == NoPlayer = True
  _ == _ = False

export
Cast PlayerType JSON where
  cast (HumanPlayer xs) =
    JObject [ ("tag", JString "HumanPlayer")
            , ("playerKey", cast xs)
            ]
  cast RobotPlayer =
    JObject [ ("tag", JString "RobotPlayer") ]
  cast NoPlayer =
    JObject [ ("tag", JString "NoPlayer") ]

public export
data GamesEvent : Type where
   NewGameCreated : (gameId : Id) -> GamesEvent
   PlayerJoined :  (playerKey : Id) -> (side : Side) -> (gameId : Id) ->  GamesEvent
   PlayerReJoined :  (playerKey : Id) ->  (side : Side) -> (gameId : Id) ->  (events : List GamesEvent) -> GamesEvent
   GameStarted :  (gameId : Id) -> GamesEvent
   PlayerPlayed :  (playerKey : Id) -> (gameId : Id) ->  (segment : GameSegment) -> (result : ActionResult segment) -> GamesEvent
   PlayerLeft :  (playerKey : Id) -> (side : Side) -> (gameId : Id) ->  GamesEvent

partial
export
Show GamesEvent where
  show (NewGameCreated gameId) =
    "NewGameCreated { gameId = " ++ show gameId ++ " }"
  show (PlayerJoined playerKey side gameId) =
    "PlayerJoined { playerKey = " ++ show playerKey ++ ", side = " ++ show side ++ ", gameId = " ++ show gameId ++ " }"
  show (PlayerReJoined playerKey side gameId events) =
    "PlayerReJoined { playerKey = " ++ show playerKey ++ ", side = " ++ show side ++ ", gameId = " ++ show gameId ++ ", events = " ++ show events ++ " }"
  show (GameStarted gameId) =
    "GameStarted { gameId = " ++ show gameId ++ " }"
  show (PlayerPlayed playerKey gameId segment result) =
    "PlayerPlayed { playerKey = " ++ show playerKey ++ ", gameId = " ++ show gameId ++ ", segment = " ++ show segment ++ ", result = " ++ show result ++ " }"
  show (PlayerLeft playerKey side gameId) =
    "PlayerLeft { playerKey = " ++ show playerKey ++ ", side = " ++ show side ++ ", gameId = " ++ show gameId ++ " }"

export
Eq GamesEvent where
  (NewGameCreated gameId) == (NewGameCreated gameId') = gameId == gameId'
  (PlayerJoined playerKey side gameId) == (PlayerJoined playerKey' side' gameId') =
    playerKey == playerKey' && side == side' && gameId == gameId'
  (PlayerReJoined playerKey side gameId events) == (PlayerReJoined playerKey' side' gameId' events') =
    playerKey == playerKey' && side == side' && gameId == gameId' -- && events == events'
  (GameStarted gameId) == GameStarted gameId' =
    gameId == gameId'
  (PlayerPlayed playerKey gameId segment result) == (PlayerPlayed playerKey' gameId' segment' result') with (decEq segment segment')
    (PlayerPlayed playerKey gameId segment result) == (PlayerPlayed playerKey' gameId' segment' result') | Yes prf = playerKey == playerKey' && gameId == gameId' && result == rewrite prf in result'
    (PlayerPlayed playerKey gameId segment result) == (PlayerPlayed playerKey' gameId' segment' result') | No _ = False
  (PlayerLeft playerKey side gameId) == (PlayerLeft playerKey' side' gameId') =
    playerKey == playerKey' && side == side' && gameId == gameId'
  _ == _ = False

convertToJSON : List GamesEvent -> JSON

export
Cast GamesEvent JSON where
  cast (NewGameCreated gameId) =
    JObject [ ("tag", JString "NewGameCreated"), ("gameId", cast gameId) ]
  cast (PlayerJoined playerKey side gameId) =
    JObject [ ("tag", JString "PlayerJoined"), ("side", cast side), ("gameId", cast gameId) ]
  cast (PlayerReJoined playerKey side gameId events) =
    JObject [ ("tag", JString "PlayerReJoined"), ("side", cast side), ("gameId", cast gameId), ("events", convertToJSON events) ]
  cast (GameStarted gameId) =
    JObject [ ("tag", JString "GameStarted"), ("gameId", cast gameId) ]
  cast (PlayerPlayed playerKey gameId segment result) =
    JObject [ ("tag", JString "PlayerPlayed"), ("gameId", cast gameId), ("segment", cast segment), ("result", cast result) ]
  cast (PlayerLeft playerKey side gameId) =
    JObject [ ("tag", JString "PlayerLeft"), ("side", cast side), ("gameId", cast gameId) ]

convertToJSON events =
  JArray (doConvert events)
  where
    doConvert : List GamesEvent -> List JSON
    doConvert [] = []
    doConvert (x :: xs) = cast x :: doConvert xs

export
FromJSON GamesEvent where
  fromJSON = withObject "GamesEvent" $ \ obj => do
     tag <- (.:) {a = String} obj "tag"
     case tag of
        "NewGameCreated" => NewGameCreated <$> (obj .: "gameId")
        "PlayerJoined" =>  [| PlayerJoined (obj .: "playerKey") (obj .: "side") (obj .: "gameId") |]
        "PlayerLeft" =>  [| PlayerLeft (obj .: "playerKey") (obj .: "side") (obj .: "gameId") |]
        _ => fail $ "Unknown tag: " ++ tag

  -- (JObject [ ("tag", JString "NewGameCreated"), ("gameId", gameId) ]) = ?hole
  -- fromJSON (JObject [ ("tag", JString "PlayerJoined"), ("side", side), ("gameId", gameId) ]) = ?hole1
  -- fromJSON (JObject [ ("tag", JString "GameStarted"), ("gameId", gameId) ]) = ?hole2
  -- fromJSON (JObject [ ("tag", JString "PlayerPlayed"), ("gameId", gameId), ("segment", segment), ("result", result) ]) = ?hole3
  -- fromJSON (JObject [ ("tag", JString "PlayerLeft"), ("side", side), ("gameId", gameId) ]) = ?hole4
  -- fromJSON j = Left $ "Unknown JSON event: " ++ show j

public export
record SingleGame where
  constructor MkSingleGame
  gameId : Id
  axisPlayer : PlayerType
  alliesPlayer : PlayerType
  theGame : Game
  ||| All events accumulated on this game
  events : Lazy (List GamesEvent)


export
Cast SingleGame JSON where
  cast (MkSingleGame gameId axisPlayer alliesPlayer theGame events) =
    JObject [ ( "tag", JString "SingleGame")
            , ("gameId", cast gameId)
            , ("axisPlayer", cast axisPlayer)
            , ("alliesPlayer", cast alliesPlayer)
            , ("segment", cast $ makeCurrentSegment theGame)
            ]


export
Games : Type
Games = SortedMap Id SingleGame

export
Cast Games JSON where
  cast g = JObject $ ( "tag", JString "Games") :: map (\ (k,v) => (show k, cast v)) (SortedMap.toList g)


record GamesList where
  constructor MkGamesList
  games : List SingleGame

Cast GamesList JSON where
  cast (MkGamesList gs) = JObject [ ( "tag", JString "GamesList") , ("games", cast gs) ]

export
makeGameCommand : Games -> JSON -> Either String GameCommand
makeGameCommand _ (JObject [ ("tag", JString "NewGame"), ("gameId", JString gameId) ]) =
  makeId gameId >>= Right . NewGame
makeGameCommand _ (JObject [ ("tag", JString "StartGame"), ("gameId", JString gameId) ]) =
  makeId gameId >>= Right . StartGame
makeGameCommand _ (JObject [ ("tag", JString "ListGames") ]) =
  Right ListGames
makeGameCommand _ (JObject [ ("tag", JString "JoinGame"), ("gameId", JString gameId) , ("side", side) ]) = do
  gid <- makeId gameId
  sd <- makeSide side
  pure $ JoinGame gid sd
makeGameCommand games (JObject [ ("tag", JString "Action"), ("gameId", JString gameId) , ("action", action) ]) = do
  gid <- makeId gameId
  case lookup gid games of
    Nothing => Left $ "Unknown gameId: " ++ show gid
    Just (MkSingleGame _ _ _ game _) =>
       makePlayerAction game action >>= Right . Action gid
makeGameCommand games (JObject [ ("tag", JString "Bye"), ("gameId", JString gameId) ]) = do
  gid <- makeId gameId
  pure $ Bye gid
makeGameCommand _ json = Left $ "Unknown command " ++ show @{Idris} json


export
(.id) : GamesEvent -> Id
(NewGameCreated gameId).id  = gameId
(PlayerJoined _ _ gameId).id  =  gameId
(PlayerReJoined _ _ gameId _).id  =  gameId
(GameStarted gameId).id  =  gameId
(PlayerPlayed _ gameId _ result).id  = gameId
(PlayerLeft _ _ gameId).id  = gameId


export
data GamesError =
   UnknownGame Id
  | GameAlreadyExists Id
  | GameIncomplete Id
  | UnknownPlayer Id
  | SideTaken Side Id
  | InvalidSegment GameSegment GameSegment Id Id

export
Cast GamesError JSON where
  cast (UnknownGame gameId) =
    JObject [ ("tag", JString "UnknownGame"), ("gameId", cast gameId) ]
  cast (GameAlreadyExists gameId) =
    JObject [ ("tag", JString "GameAlreadyExists"), ("gameId", cast gameId) ]
  cast (GameIncomplete gameId) =
    JObject [ ("tag", JString "GameIncomplete"), ("gameId", cast gameId) ]
  cast (UnknownPlayer playerKey) =
    JObject [ ("tag", JString "UnknownPlayer"), ("playerKey", cast playerKey) ]
  cast (SideTaken side playerKey) =
    JObject [ ("tag", JString "SideTaken"), ("side", cast side), ("playerKey", cast playerKey) ]
  cast (InvalidSegment actual expected playerKey gameId ) =
    JObject [ ("tag", JString "InvalidSegment"), ("actual", cast actual), ("expected", cast expected), ("playerKey", cast playerKey), ("gameId", cast gameId) ]

public export
data GamesResult : (0 games : Games) -> Type where
   GamesResEvent : (event : GamesEvent) -> GamesResult games
   GamesResQuery : Cast result JSON => result -> GamesResult games
   GamesResError  : GamesError -> GamesResult games

export
Cast (GamesResult games) JSON where
   cast (GamesResEvent event) = JObject [ ("tag", JString "GamesResEvent"), ("event", cast event) ]
   cast (GamesResQuery result) = JObject [ ("tag", JString "GamesResQuery"), ("result", cast result) ]
   cast (GamesResError error) = JObject [ ("tag", JString "GamesResError"), ("error", cast error) ]

actAction : {gameSegment: GameSegment} -> PlayerAction gameSegment -> SingleGame -> Id -> Id -> (games : Games) -> GamesResult games
actAction {gameSegment} action single@(MkSingleGame xs axisPlayer alliesPlayer theGame _) playerKey gameId games =
  case action of
      Qry q => let result = query theGame q
               in GamesResQuery result
      Cmd c =>
         case decEq (curSegment theGame) gameSegment of
           (Yes prf) =>  let result = handleAction theGame $ rewrite prf in action
                         in GamesResEvent $ PlayerPlayed playerKey gameId (curSegment theGame) result
           (No contra) => GamesResError $ InvalidSegment gameSegment (curSegment theGame) playerKey gameId

data Removed : Type where
  MkRemoved : (pk : Id) -> (side : Side) -> Removed
  NotRemoved : (pk : Id) -> Removed

removePlayerFromGame : Id -> SingleGame -> Removed
removePlayerFromGame playerKey single@(MkSingleGame gameId (HumanPlayer xs) (HumanPlayer ys) theGame _) =
  if playerKey == xs
  then MkRemoved playerKey Axis
  else if playerKey == ys
       then MkRemoved playerKey Allies
       else NotRemoved playerKey
removePlayerFromGame playerKey single@(MkSingleGame gameId (HumanPlayer xs) _ theGame _) =
  if playerKey == xs
  then MkRemoved playerKey Axis
  else NotRemoved playerKey
removePlayerFromGame playerKey single@(MkSingleGame gameId _ (HumanPlayer xs) theGame _) =
  if playerKey == xs
  then MkRemoved playerKey Allies
  else NotRemoved playerKey
removePlayerFromGame playerKey _ = NotRemoved playerKey

joinGame : SingleGame -> Id -> Side -> Either GamesError GamesEvent
joinGame game@(MkSingleGame gameId (HumanPlayer xs) NoPlayer theGame events) playerKey Axis =
      if xs == playerKey
       then Right $ PlayerJoined playerKey Axis gameId
       else Left $ SideTaken Axis playerKey
joinGame (MkSingleGame gameId NoPlayer (HumanPlayer xs) theGame events) playerKey Allies =
      if xs == playerKey
       then Right $ PlayerJoined playerKey Allies gameId
       else Left $ SideTaken Allies playerKey
joinGame game@(MkSingleGame gameId (HumanPlayer xs) _ theGame events) playerKey Axis =
      if xs == playerKey
       then Right $ PlayerReJoined playerKey Axis gameId events
       else Left $ SideTaken Axis playerKey
joinGame (MkSingleGame gameId _ (HumanPlayer xs) theGame events) playerKey Allies =
      if xs == playerKey
       then Right $ PlayerReJoined playerKey Allies gameId events
       else Left $ SideTaken Allies playerKey
joinGame (MkSingleGame gameId _ alliesPlayer theGame _) playerKey Axis =
    Right $ PlayerJoined playerKey Axis gameId
joinGame (MkSingleGame gameId axisPlayer _ theGame _) playerKey Allies =
    Right $ PlayerJoined playerKey Allies gameId

||| Interpret @GameCommand@, returning a @GamesResult@
export
act : Id -> GameCommand -> (games : Games) -> GamesResult games
act playerKey (NewGame gameId) games =
   case lookup gameId games of
      Just _ =>
        GamesResError $ GameAlreadyExists gameId
      Nothing =>
        GamesResEvent $ NewGameCreated gameId
act playerKey (StartGame gameId) games =
   case lookup gameId games of
      Just (MkSingleGame xs axisPlayer alliesPlayer theGame events) =>
         if axisPlayer == NoPlayer || alliesPlayer == NoPlayer
         then GamesResError $ GameIncomplete gameId
         else GamesResEvent $ GameStarted gameId
      Nothing => GamesResError $ UnknownGame gameId
act playerKey ListGames games =
  GamesResQuery $ MkGamesList $ values games
act playerKey (JoinGame gameId side) games =
   case lookup gameId games of
      Just game => case joinGame game playerKey side of
                    Left err => GamesResError err
                    Right event => GamesResEvent event
      Nothing => GamesResError $ UnknownGame gameId
act playerKey (Bye gameId) games =
  case SortedMap.lookup gameId games of
    Nothing => GamesResError $ UnknownGame gameId
    Just single =>
      case removePlayerFromGame playerKey single of
         MkRemoved playerKey side => GamesResEvent $ PlayerLeft playerKey side gameId
         NotRemoved playerKey => GamesResError (UnknownPlayer playerKey)
act playerKey (Action {gameSegment} gameId action) games =
   case SortedMap.lookup gameId games of
     Nothing => GamesResError $ UnknownGame gameId
     (Just single@(MkSingleGame _ _ _ game _)) =>
       actAction action single playerKey gameId games

||| Apply a @GamesResult@ to current state of @Games@
export
apply : (games : Games) -> GamesResult games -> Games
apply games (GamesResEvent  (NewGameCreated gameId)) =
 let newGame = MkSingleGame gameId NoPlayer NoPlayer initialGame []
 in insert gameId newGame games
apply games (GamesResEvent (PlayerJoined playerKey side gameId)) =
 case SortedMap.lookup gameId games of
   Just game =>
     case side of
       Axis => insert game.gameId ({ axisPlayer := HumanPlayer playerKey} game) games
       Allies => insert game.gameId ({ alliesPlayer := HumanPlayer playerKey} game) games
   Nothing => games
apply games (GamesResEvent (PlayerReJoined playerKey side gameId _)) =
 games
apply games (GamesResEvent (GameStarted gameId)) =
 games
apply games (GamesResEvent event@(PlayerPlayed playerKey gameId segment result)) =
   case SortedMap.lookup gameId games of
     Nothing => games
     (Just single@(MkSingleGame _ axis allies game events)) =>
       case result of
          (ResEvent e) =>
            -- TODO this is done twice, once here and when we run the command to
            -- create the event, there should be a way to relate both?
            case decEq (curSegment game) segment of
              Yes prf => let game' = apply game $ rewrite prf in e
                         in  insert gameId ({theGame := game', events := event :: events} single) games
              No contra => games
          (ResError x) => games
          (ResQuery x) => games
apply games (GamesResEvent (PlayerLeft playerKey side gameId)) =
   case SortedMap.lookup gameId games of
     Nothing => games
     (Just single) =>
        let newGame = case side of
                       Axis => { axisPlayer := NoPlayer} single
                       Allies => { alliesPlayer := NoPlayer} single
        in  insert gameId newGame games
apply games (GamesResError x) = games
apply games (GamesResQuery  _) = games

export
interpret : Id -> GameCommand -> (games : Games) -> (GamesResult games, Games)
interpret clientId command games =
  let res = act clientId command games
  in (res, apply games res)


export
initialGames : Games
initialGames = empty
