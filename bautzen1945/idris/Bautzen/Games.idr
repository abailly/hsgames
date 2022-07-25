||| Stateful protocol for managing players and games before they actually start
module Bautzen.Games

import Bautzen.Game
import Bautzen.Game.Core
import Data.SortedMap
import Data.Vect
import Decidable.Equality

%default total

public export
Id : Type
Id = Vect 8 Char

||| Lifecycle protocol for a single game.
|||
||| The @gameId@ type parameter is used to distinguish commands for
||| each different managed games.
public export
data GameCommand : (gameId : Id) -> Type where

  ||| Create a new game with given id.
  ||| TODO: what if game with same id exists?
  NewGame : (newGameId : Id) -> GameCommand newGameId

  ||| A player identified by @playerKey@ joins given game
  JoinGame : (playerKey : Id) -> (side : Side) -> GameCommand gameId

  ||| Game-specific action
  Action : { gameSegment : GameSegment} -> PlayerAction gameSegment -> GameCommand gameId

  ||| Given player leaves game
  Bye : (playerKey : Id) -> GameCommand gameId

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

applyAction : {gameSegment: GameSegment} -> PlayerAction gameSegment -> SingleGame -> Id -> Games -> Games
applyAction {gameSegment} act single@(MkSingleGame xs axisPlayer alliesPlayer theGame) gameId games with (decEq (curSegment theGame) gameSegment)
  applyAction act single@(MkSingleGame xs axisPlayer alliesPlayer theGame) gameId games | (Yes prf) =
    let res = handleAction theGame $ rewrite prf in act
        game' = applyResult theGame res
    in insert gameId  ({ theGame := game' } single) games
  applyAction act single@(MkSingleGame xs axisPlayer alliesPlayer theGame) gameId games | (No contra) = games

data GamesEvent : (gameId : Id) -> Type where
   NewGameCreated : (game : SingleGame) -> GamesEvent (game.gameId)
   PlayerJoined :  (playerKey : Id) -> (game : SingleGame) ->  GamesEvent (game.gameId)
   PlayerPlayed :  (playerKey : Id) -> (result : ActionResult (curSegment game)) -> GamesEvent gameId
   PlayerLeft :  (playerKey : Id) -> (game : SingleGame) ->  GamesEvent gameId

data GamesError =
   UnknownGame Id
  | UnknownPlayer Id
  | SideTaken Side Id

data GamesResult : Type where
   GamesResEvent : { gameId : Id } -> (event : GamesEvent gameId) -> GamesResult
   GamesResError  : GamesError -> GamesResult


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
interpret : { gameId : Id } -> GameCommand gameId -> Games -> GamesResult
interpret (NewGame gameId) games =
  GamesResEvent $ NewGameCreated $ MkSingleGame gameId NoPlayer NoPlayer initialGame
interpret (JoinGame playerKey Axis) games =
   case lookup gameId games of
      Just (MkSingleGame gameId NoPlayer alliesPlayer theGame) =>
        GamesResEvent $ PlayerJoined playerKey (MkSingleGame gameId (HumanPlayer playerKey) alliesPlayer theGame)
      Just _ =>
        GamesResError $ SideTaken Axis gameId
      Nothing => GamesResError $ UnknownGame gameId
interpret (JoinGame playerKey Allies) games =
   case lookup gameId games of
      Just (MkSingleGame gameId axisPlayer NoPlayer theGame) =>
        GamesResEvent $ PlayerJoined playerKey (MkSingleGame gameId axisPlayer (HumanPlayer playerKey) theGame)
      Just _ =>
        GamesResError $ SideTaken Allies gameId
      Nothing => GamesResError $ UnknownGame gameId
interpret (Bye playerKey) games =
  case SortedMap.lookup gameId games of
    Nothing => GamesResError $ UnknownGame gameId
    Just single =>
      case removePlayerFromGame playerKey single of
         Just game => GamesResEvent $ PlayerLeft {gameId} playerKey game
         Nothing => GamesResError (UnknownPlayer playerKey)
interpret _ _ = ?hole
--        insert gameId  games
--      _ => games
-- interpret (JoinGame playerKey Allies) games =
--   case lookup gameId games of
--      Just (MkSingleGame gameId axisPlayer NoPlayer theGame) =>
--        insert gameId (MkSingleGame gameId axisPlayer (HumanPlayer playerKey) theGame) games
--      _ => games
-- interpret (Action {gameSegment} act) games =
--   case SortedMap.lookup gameId games of
--     Nothing => games
--     (Just single@(MkSingleGame _ _ _ game)) =>
--       applyAction act single gameId games
-- interpret (Bye playerKey) games =
--   case SortedMap.lookup gameId games of
--     Nothing => games
--     (Just single@(MkSingleGame gameId (HumanPlayer axisId) alliesPlayer theGame))
--         => if playerKey == axisId
--            then insert gameId (MkSingleGame gameId NoPlayer alliesPlayer theGame) games
--            else games
--     (Just single@(MkSingleGame gameId axisPlayer (HumanPlayer alliesId) theGame))
--         => if playerKey == alliesId
--            then insert gameId (MkSingleGame gameId axisPlayer NoPlayer theGame) games
--            else games
--     _ => games
