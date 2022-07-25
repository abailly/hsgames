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

  ||| Create a new game with given number of human and robot players
  NewGame : (numHumans : Nat) -> (numRobots : Nat) -> (gameId : Id) -> GameCommand gameId

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
    let res = handleAction theGame $ rewrite__impl PlayerAction prf act
        game' = applyResult theGame res
    in insert gameId  ({ theGame := game' } single) games
  applyAction act single@(MkSingleGame xs axisPlayer alliesPlayer theGame) gameId games | (No contra) = games

||| Interpret @GameCommand@
interpret : { gameId : Id } -> GameCommand gameId -> Games -> Games
interpret (NewGame numHumans numRobots gameId) games =
  insert gameId (MkSingleGame gameId NoPlayer NoPlayer initialGame) games
interpret (JoinGame playerKey Axis) games =
  case lookup gameId games of
     Just (MkSingleGame gameId NoPlayer alliesPlayer theGame) =>
       insert gameId (MkSingleGame gameId (HumanPlayer playerKey) alliesPlayer theGame) games
     _ => games
interpret (JoinGame playerKey Allies) games =
  case lookup gameId games of
     Just (MkSingleGame gameId axisPlayer NoPlayer theGame) =>
       insert gameId (MkSingleGame gameId axisPlayer (HumanPlayer playerKey) theGame) games
     _ => games
interpret (Action {gameSegment} act) games =
  case SortedMap.lookup gameId games of
    Nothing => games
    (Just single@(MkSingleGame _ _ _ game)) =>
      applyAction act single gameId games
interpret (Bye playerKey) games =
  case SortedMap.lookup gameId games of
    Nothing => games
    (Just single@(MkSingleGame gameId (HumanPlayer axisId) alliesPlayer theGame))
        => if playerKey == axisId
           then insert gameId (MkSingleGame gameId NoPlayer alliesPlayer theGame) games
           else games
    (Just single@(MkSingleGame gameId axisPlayer (HumanPlayer alliesId) theGame))
        => if playerKey == alliesId
           then insert gameId (MkSingleGame gameId axisPlayer NoPlayer theGame) games
           else games
    _ => games
