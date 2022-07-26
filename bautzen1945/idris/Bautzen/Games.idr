||| Stateful protocol for managing players and games before they actually start
module Bautzen.Games

import Bautzen.Game
import Bautzen.Game.Core
import Bautzen.REPL.JSON
import Data.SortedMap
import Data.String.Parser
import Data.Vect
import Decidable.Equality
import Language.JSON

%default total

public export
Id : Type
Id = Vect 8 Char

makeId : String -> Either String Id
makeId s =
  fst <$> parse idParser s
  where
    idParser : Parser Id
    idParser = ntimes 8 alphaNum

Cast Id JSON where
  cast = JString . pack . toList

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
  JoinGame : (gameId : Id) -> (playerKey : Id) -> (side : Side) -> GameCommand

  ||| Game-specific action
  Action : { gameSegment : GameSegment} -> (gameId : Id) -> (playerKey : Id) -> PlayerAction gameSegment -> GameCommand

  ||| Given player leaves game
  Bye : (gameId : Id) ->  (playerKey : Id) -> GameCommand

public  export
data PlayerType =
     HumanPlayer Id
     | RobotPlayer
     | NoPlayer

public export
record SingleGame where
  constructor MkSingleGame
  gameId : Id
  axisPlayer : PlayerType
  alliesPlayer : PlayerType
  theGame : Game

export
Games : Type
Games = SortedMap Id SingleGame

export
covering
makeGameCommand : Games -> JSON -> Either String GameCommand
makeGameCommand _ (JObject [ ("tag", JString "NewGame"), ("gameId", JString gameId) ]) =
  makeId gameId >>= Right . NewGame
makeGameCommand _ (JObject [ ("tag", JString "JoinGame"), ("gameId", JString gameId) , ("playerKey", JString playerKey), ("side", side) ]) = do
  gid <- makeId gameId
  pk <- makeId playerKey
  sd <- makeSide side
  pure $ JoinGame gid pk sd
makeGameCommand games (JObject [ ("tag", JString "Action"), ("gameId", JString gameId) , ("playerKey", JString playerKey), ("action", action) ]) = do
  gid <- makeId gameId
  pk <- makeId playerKey
  case lookup gid games of
    Nothing => Left $ "Unknown gameId: " ++ show gid
    Just (MkSingleGame _ _ _ game) =>
       makePlayerAction game action >>= Right . Action gid pk
makeGameCommand games (JObject [ ("tag", JString "Bye"), ("gameId", JString gameId), ("playerKey", JString playerKey) ]) = do
  gid <- makeId gameId
  pk <- makeId playerKey
  pure $ Bye gid pk
makeGameCommand _ json = Left $ "Unknown command " ++ show json

Cast GameCommand JSON where
  cast (NewGame newGameId) =
    JObject [ ("tag", JString "NewGame"), ("gameId", cast newGameId) ]
  cast (JoinGame gameId playerKey side) =
    JObject [ ("tag", JString "JoinGame"), ("gameId", cast gameId) , ("playerKey", cast playerKey), ("side", cast side) ]
  cast (Action gameId playerKey action) =
    JObject [ ("tag", JString "Action"), ("gameId", cast gameId) , ("playerKey", cast playerKey), ("action", cast action) ]
  cast (Bye gameId playerKey) =
    JObject [ ("tag", JString "Bye"), ("gameId", cast gameId), ("playerKey", cast playerKey) ]

data GamesEvent : (gameId : Id) -> Type where
   NewGameCreated : (game : SingleGame) -> GamesEvent (game.gameId)
   PlayerJoined :  (playerKey : Id) -> (game : SingleGame) ->  GamesEvent (game.gameId)
   PlayerPlayed :  (playerKey : Id) -> (game : Game) ->  (result : ActionResult segment) -> GamesEvent gameId
   PlayerLeft :  (playerKey : Id) -> (game : SingleGame) ->  GamesEvent gameId

data GamesError =
   UnknownGame Id
  | UnknownPlayer Id
  | SideTaken Side Id
  | InvalidSegment GameSegment GameSegment Id Id

public export
data GamesResult : (0 games : Games) -> Type where
   GamesResEvent : { gameId : Id } -> (event : GamesEvent gameId) -> GamesResult games
   GamesResError  : GamesError -> GamesResult games

actAction : {gameSegment: GameSegment} -> PlayerAction gameSegment -> SingleGame -> Id -> Id -> (games : Games) -> GamesResult games
actAction {gameSegment} act single@(MkSingleGame xs axisPlayer alliesPlayer theGame) playerKey gameId games with (decEq (curSegment theGame) gameSegment)
  actAction act single@(MkSingleGame xs axisPlayer alliesPlayer theGame) playerKey gameId games | (Yes prf) =
    let result = handleAction theGame $ rewrite prf in act
        game' =  applyResult theGame result
    in GamesResEvent $ PlayerPlayed {gameId} playerKey game' result
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
interpret : GameCommand -> (games : Games) -> GamesResult games
interpret (NewGame gameId) games =
  GamesResEvent $ NewGameCreated $ MkSingleGame gameId NoPlayer NoPlayer initialGame
interpret (JoinGame gameId playerKey Axis) games =
   case lookup gameId games of
      Just (MkSingleGame gameId NoPlayer alliesPlayer theGame) =>
        GamesResEvent $ PlayerJoined playerKey (MkSingleGame gameId (HumanPlayer playerKey) alliesPlayer theGame)
      Just _ =>
        GamesResError $ SideTaken Axis gameId
      Nothing => GamesResError $ UnknownGame gameId
interpret (JoinGame gameId playerKey Allies) games =
   case lookup gameId games of
      Just (MkSingleGame gameId axisPlayer NoPlayer theGame) =>
        GamesResEvent $ PlayerJoined playerKey (MkSingleGame gameId axisPlayer (HumanPlayer playerKey) theGame)
      Just _ =>
        GamesResError $ SideTaken Allies gameId
      Nothing => GamesResError $ UnknownGame gameId
interpret (Bye gameId playerKey) games =
  case SortedMap.lookup gameId games of
    Nothing => GamesResError $ UnknownGame gameId
    Just single =>
      case removePlayerFromGame playerKey single of
         Just game => GamesResEvent $ PlayerLeft {gameId} playerKey game
         Nothing => GamesResError (UnknownPlayer playerKey)
interpret (Action {gameSegment} gameId playerKey act) games =
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
            insert gameId (MkSingleGame gameId axis allies game') games
          (ResError x) => games
          (ResQuery x) => games
apply games (GamesResEvent {gameId} (PlayerLeft playerKey game)) =
  insert gameId game games
apply games (GamesResError x) = games

export
initialGames : Games
initialGames = empty
