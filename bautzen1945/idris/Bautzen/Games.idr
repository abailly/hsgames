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

||| Lifecycle protocol for a single game.
|||
||| The @gameId@ type parameter is used to distinguish commands for
||| each different managed games.
public export
data GameCommand : Type where

  ||| Create a new game with given id.
  ||| TODO: what if game with same id exists?
  NewGame : (newGameId : Id) -> GameCommand

  ||| A player identified by @playerKey@ joins given game
  JoinGame : (gameId : Id) -> (side : Side) -> GameCommand

  ||| Game-specific action
  Action : { gameSegment : GameSegment} -> (gameId : Id) -> PlayerAction gameSegment -> GameCommand

  ||| Given player leaves game
  Bye : (gameId : Id) ->  GameCommand

public  export
data PlayerType =
     HumanPlayer Id
     | RobotPlayer
     | NoPlayer

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
data GamesEvent : (gameId : Id) -> Type where
   NewGameCreated : (game : SingleGame) -> GamesEvent (game.gameId)
   PlayerJoined :  (playerKey : Id) -> (game : SingleGame) ->  GamesEvent (game.gameId)
   PlayerPlayed :  (playerKey : Id) -> (game : SingleGame) ->  (result : ActionResult segment) -> GamesEvent gameId
   PlayerLeft :  (playerKey : Id) -> (game : SingleGame) ->  GamesEvent gameId

export
Cast (GamesEvent gid) JSON where
  cast (NewGameCreated game) =
    JObject [ ("tag", JString "NewGameCreated"), ("game", cast game.gameId) ]
  cast (PlayerJoined playerKey game) =
    JObject [ ("tag", JString "PlayerJoined"), ("gameId", cast game.gameId) ]
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
   GamesResEvent : { gameId : Id } -> (event : GamesEvent gameId) -> GamesResult games
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
    in GamesResEvent $ PlayerPlayed {gameId} playerKey ({ theGame := game' } single) result
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
interpret : Id -> GameCommand -> (games : Games) -> GamesResult games
interpret playerKey (NewGame gameId) games =
   case lookup gameId games of
      Just _ =>
        GamesResError $ GameAlreadyExists gameId
      Nothing =>
        GamesResEvent $ NewGameCreated $ MkSingleGame gameId NoPlayer NoPlayer initialGame
interpret playerKey (JoinGame gameId Axis) games =
   case lookup gameId games of
      Just (MkSingleGame gameId NoPlayer alliesPlayer theGame) =>
        GamesResEvent $ PlayerJoined playerKey (MkSingleGame gameId (HumanPlayer playerKey) alliesPlayer theGame)
      Just _ =>
        GamesResError $ SideTaken Axis gameId
      Nothing => GamesResError $ UnknownGame gameId
interpret playerKey (JoinGame gameId Allies) games =
   case lookup gameId games of
      Just (MkSingleGame gameId axisPlayer NoPlayer theGame) =>
        GamesResEvent $ PlayerJoined playerKey (MkSingleGame gameId axisPlayer (HumanPlayer playerKey) theGame)
      Just _ =>
        GamesResError $ SideTaken Allies gameId
      Nothing => GamesResError $ UnknownGame gameId
interpret playerKey (Bye gameId) games =
  case SortedMap.lookup gameId games of
    Nothing => GamesResError $ UnknownGame gameId
    Just single =>
      case removePlayerFromGame playerKey single of
         Just game => GamesResEvent $ PlayerLeft {gameId} playerKey game
         Nothing => GamesResError (UnknownPlayer playerKey)
interpret playerKey (Action {gameSegment} gameId act) games =
   case SortedMap.lookup gameId games of
     Nothing => GamesResError $ UnknownGame gameId
     (Just single@(MkSingleGame _ _ _ game)) =>
       actAction act single playerKey gameId games

||| Apply a @GamesResult@ to current state of @Games@
export
apply : (games : Games) -> GamesResult games -> Games
apply games (GamesResEvent {gameId = (game .gameId)} (NewGameCreated game)) =
 insert game.gameId game games
apply games (GamesResEvent {gameId = (game .gameId)} (PlayerJoined playerKey game)) =
 insert game.gameId game games
apply games (GamesResEvent {gameId} (PlayerPlayed playerKey game' result)) =
   -- TODO: we know the gameId is in the games so we shouldn't pattern match
   case SortedMap.lookup gameId games of
     Nothing => games
     (Just single@(MkSingleGame _ axis allies game)) =>
       case result of
          (ResEvent _) =>
            insert gameId game' games
          (ResError x) => games
          (ResQuery x) => games
apply games (GamesResEvent {gameId} (PlayerLeft playerKey game)) =
  insert gameId game games
apply games (GamesResError x) = games

export
initialGames : Games
initialGames = empty
