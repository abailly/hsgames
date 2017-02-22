{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- = How to Play Acquire
--
-- These game rules are stolen from <http://www.cs.cmu.edu/~lanthony/classes/SEng/Design/acquire.html this page> which
-- is part of CS curriculum.
--
-- The main object of Acquire is to become the wealthiest player by the end of the game.  This is done by forming
-- hotel chains, shrewdly buying the right stock at the right time, merging chains to obtain capital and adding
-- hotels to the chains in which you have controlling interest to increase their value.
module Acquire.Game (
  -- * Types
  Game(..), GameBoard, Order(Cancel), GameId,
  -- * High-level interface to game play
  -- ** Query/update game state
  currentPlayer, newGame, play, possiblePlay, highlightPlayableTiles,
  -- ** Main Game Loop
  PlayerInput(..), Handler, Message(..), initialisedGame, interpretCommand
    ) where

import           Acquire.Cells
import           Acquire.Game.Core
import           Acquire.Game.Play
import           Acquire.Game.Turn
import           Acquire.Player
import           Control.Monad.Prompt
import           Data.Aeson           (ToJSON)
import           Data.Array           ((//))
import           GHC.Generics
import           System.Random


data PlayerInput a where
  GetOrder    :: Player -> Game -> PlayerInput Order
  PlayedOrder :: Player -> Game -> Order -> PlayerInput ()
  Quit        :: Game -> PlayerInput ()
  SaveGame    :: Game -> PlayerInput ()
  LoadGame    :: GameId -> PlayerInput (Maybe Game)

type Handler m a = PlayerInput a -> m a

data Message = GameState { gsPlayer :: Player, gsBoard :: GameBoard, gsPlayables ::  [Order] }
             | Played { gsPlayerName :: PlayerName, gsBoard ::  GameBoard, gsPlayed :: Order }
             | GameEnds { gsEndGame :: Game }
             deriving (Eq, Show, Read, Generic)

instance ToJSON Message

initialisedGame :: GameId -> StdGen -> [(PlayerName,PlayerType)] -> Prompt PlayerInput Game
initialisedGame gid g num = do
  loaded <- prompt $ LoadGame gid
  case loaded of
   Nothing    -> return $ newGame gid g num
   Just  game -> return game

interpretCommand :: Game -> Prompt PlayerInput Game
interpretCommand game@Game{..} = do
  prompt $ SaveGame game
  let player = currentPlayer game
  order <- prompt $ GetOrder player game
  if   order == Cancel
  then prompt (Quit game) >> return game
  else do
    let game' = play game order
    prompt $ PlayedOrder player game' order
    interpretCommand game'

highlightPlayableTiles :: GameBoard -> [ Order ] -> GameBoard
highlightPlayableTiles board []                      = board
highlightPlayableTiles board ((Place _ coord):plays) = highlightPlayableTiles board' plays
  where
    board' = board // [(coord, Cell coord Playable)]
highlightPlayableTiles board (_:plays)               = highlightPlayableTiles board plays
