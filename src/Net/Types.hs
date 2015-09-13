{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Net.Types(module Player,
                 Command(..), GameId, Connection(..), Connections, ActiveGame(..), GameDescription(..), Result(..),
                 gamesList) where

import           Control.Concurrent
import qualified Data.Map           as M
import           Game               (GameId)
import           Interpreter        (Connection (..), Connections)
import           Player
import           Pretty             hiding ((<$>))

data Command = NewGame Int Int             -- ^Starts a game with given number of human players and robots
             | StartingGame PlayerName     -- ^Notification from player he is joining runnable game
             | JoinGame PlayerName GameId  -- ^Player joins an existing game
             | ListGames
             deriving (Show, Read)


data Result = PlayerRegistered PlayerName GameId
            | NewGameStarted GameId
            | GameStarts GameId
            | GamesList [GameDescription]
            | ErrorMessage String
            deriving (Show, Read)

data ActiveGame = ActiveGame { gameId            :: GameId
                             , numberOfHumans    :: Int
                             , numberOfRobots    :: Int
                             , registeredHumans  :: Connections
                             , connectionThreads :: [ ThreadId ]
                             , gameThread        :: Maybe ThreadId
                             }

data GameDescription = GameDescription { gameDescId           :: GameId
                                       , descNumberOfHumans   :: Int
                                       , descNumberOfRobots   :: Int
                                       , descRegisteredHumans :: [ PlayerName ]
                                       , descLive             :: Bool
                                       } deriving (Show,Read,Eq)

gamesList :: ActiveGame -> GameDescription
gamesList ActiveGame{..} = GameDescription gameId numberOfHumans numberOfRobots (M.keys registeredHumans) (gameThread /= Nothing)

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
