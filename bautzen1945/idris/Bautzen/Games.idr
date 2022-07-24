||| Stateful protocol for managing players and games before they actually start
module Bautzen.Games

import Bautzen.Game
import Data.SortedMap
import Data.Vect

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
  Action : PlayerAction gameSegment -> GameCommand gameId

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

export
Games : Type
Games = SortedMap Id SingleGame

||| Interpret @GameCommand@
interpret : { gameId : Id } -> GameCommand gameId -> Games -> Games
interpret (NewGame numHumans numRobots gameId) games =
  insert gameId (MkSingleGame gameId NoPlayer NoPlayer) games
interpret (JoinGame playerKey Axis) games =
  case lookup gameId games of
     Just (MkSingleGame gameId NoPlayer alliesPlayer) =>
       insert gameId (MkSingleGame gameId (HumanPlayer playerKey) alliesPlayer) games
     _ => games
interpret (JoinGame playerKey Allies) games =
  case lookup gameId games of
     Just (MkSingleGame gameId axisPlayer NoPlayer) =>
       insert gameId (MkSingleGame gameId axisPlayer (HumanPlayer playerKey)) games
     _ => games
interpret (Action x) games = ?hol2
interpret (Bye playerKey) games = ?hol_3
