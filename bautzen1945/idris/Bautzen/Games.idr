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

%default total

||| Lifecycle protocol for games.
|||
||| The @gameId@ type parameter is used to distinguish commands for
||| each different managed games.
public export
data GameCommand : Type where

  ||| Create a new game with given id.
  NewGame : (newGameId : Id) -> GameCommand

  ||| A player identified by @playerKey@ joins given game
  JoinGame : (gameId : Id) -> (side : Side) -> GameCommand

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
record SingleGame where
  constructor MkSingleGame
  gameId : Id
  axisPlayer : PlayerType
  alliesPlayer : PlayerType
  theGame : Game

export
Cast SingleGame JSON where
  cast (MkSingleGame gameId axisPlayer alliesPlayer theGame) =
    JObject [ ( "tag", JString "SingleGame")
            , ("gameId", cast gameId)
            , ("axisPlayer", cast axisPlayer)
            , ("alliesPlayer", cast alliesPlayer)
            , ("game", cast theGame)
            ]

export
Games : Type
Games = SortedMap Id SingleGame

export
Cast Games JSON where
  cast g = JObject $ ( "tag", JString "Games") :: map (\ (k,v) => (show k, cast v)) (SortedMap.toList g)

export
makeGameCommand : Games -> JSON -> Either String GameCommand
makeGameCommand _ (JObject [ ("tag", JString "NewGame"), ("gameId", JString gameId) ]) =
  makeId gameId >>= Right . NewGame
makeGameCommand _ (JObject [ ("tag", JString "JoinGame"), ("gameId", JString gameId) , ("side", side) ]) = do
  gid <- makeId gameId
  sd <- makeSide side
  pure $ JoinGame gid sd
makeGameCommand games (JObject [ ("tag", JString "Action"), ("gameId", JString gameId) , ("action", action) ]) = do
  gid <- makeId gameId
  case lookup gid games of
    Nothing => Left $ "Unknown gameId: " ++ show @{AsString} gid
    Just (MkSingleGame _ _ _ game) =>
       makePlayerAction game action >>= Right . Action gid
makeGameCommand games (JObject [ ("tag", JString "Bye"), ("gameId", JString gameId) ]) = do
  gid <- makeId gameId
  pure $ Bye gid
makeGameCommand _ json = Left $ "Unknown command " ++ show json

export
data GamesEvent : Type where
   NewGameCreated : (game : SingleGame) -> GamesEvent
   PlayerJoined :  (playerKey : Id) -> (game : SingleGame) ->  GamesEvent
   GameStarted :  (playerKey : Id) -> (game : SingleGame) ->  GamesEvent
   PlayerPlayed :  (playerKey : Id) -> (game : SingleGame) ->  (result : ActionResult segment) -> GamesEvent
   PlayerLeft :  (playerKey : Id) -> (game : SingleGame) ->  GamesEvent

export
(.id) : GamesEvent -> Id
(NewGameCreated game).id  = game.gameId
(PlayerJoined playerKey game).id  =  game.gameId
(GameStarted playerKey game).id  =  game.gameId
(PlayerPlayed playerKey game result).id  = game.gameId
(PlayerLeft playerKey game).id  = game.gameId

export
Cast GamesEvent JSON where
  cast (NewGameCreated game) =
    JObject [ ("tag", JString "NewGameCreated"), ("game", cast game.gameId) ]
  cast (PlayerJoined playerKey game) =
    JObject [ ("tag", JString "PlayerJoined"), ("gameId", cast game.gameId) ]
  cast (GameStarted playerKey game) =
    JObject [ ("tag", JString "GameStarted"), ("gameId", cast game.gameId) ]
  cast (PlayerPlayed playerKey game result) =
    JObject [ ("tag", JString "PlayerPlayed"), ("gameId", cast game.gameId), ("result", cast result) ]
  cast (PlayerLeft playerKey game) =
    JObject [ ("tag", JString "PlayerLeft"), ("gameId", cast game.gameId) ]

export
data GamesError =
   UnknownGame Id
  | GameAlreadyExists Id
  | UnknownPlayer Id
  | SideTaken Side Id
  | InvalidSegment GameSegment GameSegment Id Id

export
Cast GamesError JSON where
  cast (UnknownGame gameId) =
    JObject [ ("tag", JString "UnknownGame"), ("gameId", cast gameId) ]
  cast (GameAlreadyExists gameId) =
    JObject [ ("tag", JString "GameAlreadyExists"), ("gameId", cast gameId) ]
  cast (UnknownPlayer playerKey) =
    JObject [ ("tag", JString "UnknownPlayer"), ("playerKey", cast playerKey) ]
  cast (SideTaken side playerKey) =
    JObject [ ("tag", JString "SideTaken"), ("side", cast side), ("playerKey", cast playerKey) ]
  cast (InvalidSegment actual expected playerKey gameId ) =
    JObject [ ("tag", JString "SideTaken"), ("actual", cast actual), ("expected", cast expected), ("playerKey", cast playerKey), ("gameId", cast gameId) ]

public export
data GamesResult : (0 games : Games) -> Type where
   GamesResEvent : (event : GamesEvent) -> GamesResult games
   GamesResError  : GamesError -> GamesResult games

export
Cast (GamesResult games) JSON where
   cast (GamesResEvent event) = JObject [ ("tag", JString "GamesResEvent"), ("event", cast event) ]
   cast (GamesResError error) = JObject [ ("tag", JString "GamesResError"), ("error", cast error) ]

actAction : {gameSegment: GameSegment} -> PlayerAction gameSegment -> SingleGame -> Id -> Id -> (games : Games) -> GamesResult games
actAction {gameSegment} act single@(MkSingleGame xs axisPlayer alliesPlayer theGame) playerKey gameId games with (decEq (curSegment theGame) gameSegment)
  actAction act single@(MkSingleGame xs axisPlayer alliesPlayer theGame) playerKey gameId games | (Yes prf) =
    let result = handleAction theGame $ rewrite prf in act
        game' =  applyResult theGame result
    in GamesResEvent $ PlayerPlayed playerKey ({ theGame := game' } single) result
  actAction act single@(MkSingleGame xs axisPlayer alliesPlayer theGame) playerKey gameId games | (No contra) =
    GamesResError $ InvalidSegment gameSegment (curSegment theGame) playerKey gameId

removePlayerFromGame : Id -> SingleGame -> Maybe SingleGame
removePlayerFromGame playerKey single@(MkSingleGame gameId (HumanPlayer xs) (HumanPlayer ys) theGame) =
  if playerKey == xs
  then Just $ { axisPlayer := NoPlayer } single
  else if playerKey == ys
       then Just $ { alliesPlayer := NoPlayer } single
       else Nothing
removePlayerFromGame playerKey single@(MkSingleGame gameId (HumanPlayer xs) _ theGame) =
  if playerKey == xs
  then Just $ { axisPlayer := NoPlayer } single
  else Nothing
removePlayerFromGame playerKey single@(MkSingleGame gameId _ (HumanPlayer xs) theGame) =
  if playerKey == xs
  then Just $ { alliesPlayer := NoPlayer } single
  else Nothing
removePlayerFromGame _ _ = Nothing

||| Interpret @GameCommand@, returning a @GamesResult@
export
act : Id -> GameCommand -> (games : Games) -> GamesResult games
act playerKey (NewGame gameId) games =
   case lookup gameId games of
      Just _ =>
        GamesResError $ GameAlreadyExists gameId
      Nothing =>
        GamesResEvent $ NewGameCreated $ MkSingleGame gameId NoPlayer NoPlayer initialGame
act playerKey (JoinGame gameId Axis) games =
   case lookup gameId games of
      Just (MkSingleGame gameId NoPlayer alliesPlayer theGame) =>
        let game = (MkSingleGame gameId (HumanPlayer playerKey) alliesPlayer theGame)
        in if alliesPlayer == NoPlayer
           then GamesResEvent $ PlayerJoined playerKey game
           else GamesResEvent $ GameStarted playerKey game
      Just _ =>
        GamesResError $ SideTaken Axis playerKey
      Nothing => GamesResError $ UnknownGame gameId
act playerKey (JoinGame gameId Allies) games =
   case lookup gameId games of
      Just (MkSingleGame gameId axisPlayer NoPlayer theGame) =>
        let game = (MkSingleGame gameId axisPlayer (HumanPlayer playerKey) theGame)
        in if axisPlayer == NoPlayer
           then GamesResEvent $ PlayerJoined playerKey game
           else GamesResEvent $ GameStarted playerKey game
      Just _ =>
        GamesResError $ SideTaken Allies gameId
      Nothing => GamesResError $ UnknownGame gameId
act playerKey (Bye gameId) games =
  case SortedMap.lookup gameId games of
    Nothing => GamesResError $ UnknownGame gameId
    Just single =>
      case removePlayerFromGame playerKey single of
         Just game => GamesResEvent $ PlayerLeft playerKey game
         Nothing => GamesResError (UnknownPlayer playerKey)
act playerKey (Action {gameSegment} gameId action) games =
   case SortedMap.lookup gameId games of
     Nothing => GamesResError $ UnknownGame gameId
     (Just single@(MkSingleGame _ _ _ game)) =>
       actAction action single playerKey gameId games

||| Apply a @GamesResult@ to current state of @Games@
export
apply : (games : Games) -> GamesResult games -> Games
apply games (GamesResEvent  (NewGameCreated game)) =
 insert game.gameId game games
apply games (GamesResEvent  (PlayerJoined playerKey game)) =
 insert game.gameId game games
apply games (GamesResEvent  (GameStarted playerKey game)) =
 insert game.gameId game games
apply games (GamesResEvent (PlayerPlayed playerKey game' result)) =
   -- TODO: we know the gameId is in the games so we shouldn't pattern match
   case SortedMap.lookup game'.gameId games of
     Nothing => games
     (Just single@(MkSingleGame _ axis allies game)) =>
       case result of
          (ResEvent _) =>
            insert game'.gameId game' games
          (ResError x) => games
          (ResQuery x) => games
apply games (GamesResEvent (PlayerLeft playerKey game)) =
  insert game.gameId game games
apply games (GamesResError x) = games

export
interpret : Id -> GameCommand -> (games : Games) -> (GamesResult games, Games)
interpret clientId command games =
  let res = act clientId command games
  in (res, apply games res)


export
initialGames : Games
initialGames = empty
