||| Stateful protocol for managing players and games before they actually start
module Bautzen.Games

import Bautzen.Game
import Bautzen.Game.Core
import Bautzen.Id
import Bautzen.REPL.JSON
import public Data.SortedMap
import Data.String.Parser
import Data.Vect
import Decidable.Equality
import JSON.Parser

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
  Action : { gameSegment : GameSegment} -> (gameId : Id) -> Show (PlayerAction gameSegment) => PlayerAction gameSegment -> GameCommand

  ||| Given player leaves game
  Bye : (gameId : Id) ->  GameCommand

export
Show GameCommand where
  show (NewGame newGameId) = "NewGame " ++ show newGameId
  show ListGames = "ListGames"
  show (JoinGame gameId side) = "JoinGames " ++ show gameId ++ " " ++ show side
  show (StartGame newGameId) = "StartGame " ++ show newGameId
  show (Action gameId x) = "Action " ++ show gameId ++ " " ++ show x
  show (Bye gameId) = "Bye " ++ show gameId

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

Show PlayerType where
  show (HumanPlayer x) = "HumanPlayer " ++ show x
  show RobotPlayer = "RobotPlayer"
  show NoPlayer = "NoPlayer"

export
ToJSON PlayerType  where
  toJSON (HumanPlayer xs) =
    object [ ("tag", string "HumanPlayer")
            , ("playerKey", toJSON xs)
            ]
  toJSON RobotPlayer =
    object [ ("tag", string "RobotPlayer") ]
  toJSON NoPlayer =
    object [ ("tag", string "NoPlayer") ]

public export
data GamesEvent : Type where
   NewGameCreated : (gameId : Id) -> GamesEvent
   PlayerJoined :  (playerKey : Id) -> (side : Side) -> (gameId : Id) ->  GamesEvent
   PlayerReJoined :  (playerKey : Id) ->  (side : Side) -> (gameId : Id) ->  (events : List GamesEvent) -> GamesEvent
   GameStarted :  (gameId : Id) -> GamesEvent
   PlayerPlayed :  (playerKey : Id) -> (gameId : Id) ->  (segment : GameSegment) -> (result : ActionResult) -> GamesEvent
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
    (PlayerPlayed playerKey gameId segment result) == (PlayerPlayed playerKey' gameId' segment' result') | Yes prf = playerKey == playerKey' && gameId == gameId' && result == result'
    (PlayerPlayed playerKey gameId segment result) == (PlayerPlayed playerKey' gameId' segment' result') | No _ = False
  (PlayerLeft playerKey side gameId) == (PlayerLeft playerKey' side' gameId') =
    playerKey == playerKey' && side == side' && gameId == gameId'
  _ == _ = False

convertToJSON : Encoder v => List GamesEvent -> v

export
ToJSON GamesEvent  where
  toJSON (NewGameCreated gameId) =
    object [ ("tag", string "NewGameCreated"), ("gameId", toJSON gameId) ]
  toJSON (PlayerJoined playerKey side gameId) =
    object [ ("tag", string "PlayerJoined"), ("playerKey", toJSON playerKey), ("side", toJSON side), ("gameId", toJSON gameId) ]
  toJSON (PlayerReJoined playerKey side gameId events) =
    object [ ("tag", string "PlayerReJoined"), ("playerKey", toJSON playerKey), ("side", toJSON side), ("gameId", toJSON gameId), ("events", convertToJSON events) ]
  toJSON (GameStarted gameId) =
    object [ ("tag", string "GameStarted"), ("gameId", toJSON gameId) ]
  toJSON (PlayerPlayed playerKey gameId segment result) =
    object [ ("tag", string "PlayerPlayed"), ("playerKey", toJSON playerKey), ("gameId", toJSON gameId), ("segment", toJSON segment), ("result", toJSON result) ]
  toJSON (PlayerLeft playerKey side gameId) =
    object [ ("tag", string "PlayerLeft"), ("playerKey", toJSON playerKey), ("side", toJSON side), ("gameId", toJSON gameId) ]


convertToJSON events =
  array (doConvert events)
  where
    doConvert : List GamesEvent -> List v
    doConvert [] = []
    doConvert (x :: xs) = toJSON x :: doConvert xs

export
FromJSON GamesEvent where
  fromJSON = withObject "GamesEvent" $ \ obj => do
     tag <- field {a = String} obj "tag"
     case tag of
        "NewGameCreated" => NewGameCreated <$> (field obj "gameId")
        "PlayerJoined" =>  [| PlayerJoined (field obj "playerKey") (field obj "side") (field obj "gameId") |]
        "PlayerReJoined" =>  [| PlayerReJoined (field obj "playerKey") (field obj "side") (field obj "gameId") (field obj "events") |]
        "GameStarted" => GameStarted <$> (field obj "gameId")
        "PlayerPlayed" => [| PlayerPlayed (field obj "playerKey") (field obj "gameId") (field obj "segment") (field obj "result") |]
        "PlayerLeft" =>  [| PlayerLeft (field obj "playerKey") (field obj "side") (field obj "gameId") |]
        _ => fail $ "Unknown tag: " ++ tag

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
ToJSON SingleGame  where
  toJSON (MkSingleGame gameId axisPlayer alliesPlayer theGame events) =
    object [ ( "tag", string "SingleGame")
            , ("gameId", toJSON gameId)
            , ("axisPlayer", toJSON axisPlayer)
            , ("alliesPlayer", toJSON alliesPlayer)
            , ("segment", toJSON $ makeCurrentSegment theGame)
            ]

export
Eq SingleGame where
  (MkSingleGame gameId axisPlayer alliesPlayer theGame events) == (MkSingleGame gameId' axisPlayer' alliesPlayer' theGame' events')  =
     gameId == gameId' &&
     axisPlayer == axisPlayer' &&
     alliesPlayer == alliesPlayer' &&
     theGame == theGame' &&
     events == events'

partial
export
Show SingleGame where
  show (MkSingleGame gameId axisPlayer alliesPlayer theGame events) =
    "MkSingleGame { " ++
      "gameId = "  ++ show gameId ++ "," ++
      "axisPlayer = "  ++ show axisPlayer ++ "," ++
      "alliesPlayer = "  ++ show alliesPlayer ++ "," ++
      "theGame = "  ++ show theGame ++ "," ++
      "events = "  ++ show events ++ "}"

public
export
Games : Type
Games = SortedMap Id SingleGame

export
ToJSON Games where
  toJSON g = object $ ( "tag", string "Games") :: map (\ (k,v) => (show k, toJSON v)) (SortedMap.toList g)


record GamesList where
  constructor MkGamesList
  games : List SingleGame

partial
export
Show GamesList where
  show (MkGamesList l) = show l

ToJSON GamesList where
  toJSON (MkGamesList gs) = object [ ( "tag", string "GamesList") , ("games", toJSON gs) ]

export
makeGameCommand : Games -> JSON -> Either String GameCommand
makeGameCommand _ (JObject [ ("tag", JString "NewGame"), ("gameId", JString gameId) ]) =
  makeId gameId >>= Right . NewGame
makeGameCommand _ (JObject [ ("tag", JString "StartGame"), ("gameId", JString gameId) ]) =
  makeId gameId >>= Right . StartGame
makeGameCommand _ (JObject [ ("tag", JString "ListGames") ]) =
  Right ListGames
makeGameCommand _ (JObject [ ("tag", JString "JoinGame"), ("gameId", JString gameId) , ("side", JString side) ]) = do
  gid <- makeId gameId
  sd <- makeSide side
  pure $ JoinGame gid sd
makeGameCommand games (JObject [ ("tag", JString "Action"), ("gameId", JString gameId) , ("action", action) ]) = do
  gid <- makeId gameId
  case lookup gid games of
    Nothing => Left $ "Unknown gameId: " ++ show gid
    Just (MkSingleGame _ _ _ game _) =>
       Action gid <$> makePlayerAction game action
makeGameCommand games (JObject [ ("tag", JString "Bye"), ("gameId", JString gameId) ]) = do
  gid <- makeId gameId
  pure $ Bye gid
makeGameCommand _ json = Left $ "Unknown command " ++ show json


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
Show GamesError where
  show (UnknownGame gameId) =
    "UnknownGame " ++ show gameId
  show (GameAlreadyExists gameId) =
    "GameAlreadyExists " ++ show gameId
  show (GameIncomplete gameId) =
    "GameIncomplete " ++ show gameId
  show (UnknownPlayer playerKey) =
    "UnknownPlayer " ++ show playerKey
  show (SideTaken side playerKey) =
    "SideTaken " ++ show side ++ " " ++ show playerKey
  show (InvalidSegment actual expected playerKey gameId ) =
    "InvalidSegment " ++ show actual ++ "  " ++ show expected ++ " " ++ show  playerKey ++ " " ++ show gameId

export
ToJSON GamesError where
  toJSON (UnknownGame gameId) =
    object [ ("tag", string "UnknownGame"), ("gameId", toJSON gameId) ]
  toJSON (GameAlreadyExists gameId) =
    object [ ("tag", string "GameAlreadyExists"), ("gameId", toJSON gameId) ]
  toJSON (GameIncomplete gameId) =
    object [ ("tag", string "GameIncomplete"), ("gameId", toJSON gameId) ]
  toJSON (UnknownPlayer playerKey) =
    object [ ("tag", string "UnknownPlayer"), ("playerKey", toJSON playerKey) ]
  toJSON (SideTaken side playerKey) =
    object [ ("tag", string "SideTaken"), ("side", toJSON side), ("playerKey", toJSON playerKey) ]
  toJSON (InvalidSegment actual expected playerKey gameId ) =
    object [ ("tag", string "InvalidSegment"), ("actual", toJSON actual), ("expected", toJSON expected), ("playerKey", toJSON playerKey), ("gameId", toJSON gameId) ]

public export
data GamesResult : Type where
   GamesResEvent : (event : GamesEvent) -> GamesResult
   GamesResQuery : Show res => ToJSON res => res -> GamesResult
   GamesResError : GamesError -> GamesResult

export
ToJSON GamesResult where
   toJSON (GamesResEvent event) = object [ ("tag", string "GamesResEvent"), ("event", toJSON event) ]
   toJSON (GamesResQuery result) = object [ ("tag", string "GamesResQuery"), ("result", toJSON result) ]
   toJSON (GamesResError error) = object [ ("tag", string "GamesResError"), ("error", toJSON error) ]

export
partial
Show GamesResult where
   show (GamesResEvent event) = "Event " ++ show event
   show (GamesResQuery x) = "Query " ++ show (show { ty = JSON} $ toJSON x)
   show (GamesResError x) = "Error " ++ show x

actAction : {gameSegment: GameSegment} -> PlayerAction gameSegment -> SingleGame -> Id -> Id -> (games : Games) -> GamesResult
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
act : Id -> GameCommand -> (games : Games) -> GamesResult
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
apply : (games : Games) -> GamesResult -> Games
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
apply games (GamesResEvent event@(PlayerPlayed playerKey gameId _ result)) =
   case SortedMap.lookup gameId games of
     Nothing => games
     (Just single@(MkSingleGame _ axis allies game events)) =>
       case result of
          (ResEvent (MkAnyEvent {seg} e)) =>
            -- TODO this is done twice, once here and when we run the command to
            -- create the event, there should be a way to relate both?
            case (decEq (curSegment game) seg) of
              Yes prf => let game' = Game.apply game $ rewrite prf in e
                                     in  insert gameId ({theGame := game', events := event :: events} single) games
              _ => games
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
interpret : Id -> GameCommand -> (games : Games) -> (GamesResult, Games)
interpret clientId command games =
  let res = act clientId command games
  in (res, apply games res)


export
initialGames : Games
initialGames = empty
