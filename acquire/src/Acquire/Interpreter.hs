{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Acquire.Interpreter where

import           Acquire.Game
import           Acquire.Player
import           Control.Monad.Prompt
import           Data.Aeson           (ToJSON)
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
