{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Acquire.Net.Types(module Acquire.Player, G.Game,
                         Command(..), G.GameId, Connection(..), Connections, ActiveGame(..), GameDescription(..), Result(..),
                         gamesList) where

import qualified Acquire.Game             as G
import           Acquire.Interpreter      (Connection (..), Connections)
import           Acquire.Player
import           Acquire.Pretty           hiding ((<$>))
import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Data.Map                 as M
import           Data.Maybe               (isJust)

data Command = NewGame Int Int             -- ^Starts a game with given number of human players and robots
             | StartingGame PlayerName     -- ^Notification from player he is joining runnable game
             | JoinGame PlayerName G.GameId  -- ^Player joins an existing game
             | ListGames
             deriving (Show, Read)


data Result = PlayerRegistered PlayerName G.GameId
            | NewGameStarted G.GameId
            | GameStarts G.GameId
            | GamesList [GameDescription]
            | ErrorMessage String
            deriving (Show, Read)

data ActiveGame = ActiveGame { gameId            :: G.GameId
                             , numberOfHumans    :: Int
                             , numberOfRobots    :: Int
                             , registeredHumans  :: Connections
                             , connectionThreads :: [ ThreadId ]
                             , gameThread        :: Maybe (Async G.Game)
                             }

data GameDescription = GameDescription { gameDescId           :: G.GameId
                                       , descNumberOfHumans   :: Int
                                       , descNumberOfRobots   :: Int
                                       , descRegisteredHumans :: [ PlayerName ]
                                       , descLive             :: Bool
                                       } deriving (Show,Read,Eq)

gamesList :: ActiveGame -> GameDescription
gamesList ActiveGame{..} = GameDescription gameId numberOfHumans numberOfRobots (M.keys registeredHumans) (isJust gameThread)

instance Pretty GameDescription where
  pretty GameDescription{..} = text gameDescId <+> int descNumberOfHumans <+> text "humans" <> char ','
                                      <+> int descNumberOfRobots <+> text "robots" <> char ','
                                      <+> list (map text descRegisteredHumans)


instance Pretty Result where
  pretty (PlayerRegistered playerName gameId) = text "registered player" <+> text playerName <+> text "with game" <+> text gameId
  pretty (NewGameStarted gameId)              = text "created new game" <+> text gameId
  pretty (GameStarts gameId)                  = text "starting game" <+> text gameId
  pretty (GamesList descs)                    = text "list of games:" <$$> vcat (map pretty descs)
  pretty (ErrorMessage msg)                   = text msg
