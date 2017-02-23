{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Acquire.Net.Types(Game,
                         Command(..), GameId, Connection(..), Connections, ActiveGame(..), GameDescription(..), Result(..),
                         gamesList) where

import           Acquire.Game
import           Acquire.Pretty           hiding ((<$>))
import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.Aeson               (ToJSON)
import qualified Data.Map                 as M
import           Data.Maybe               (isJust)
import           GHC.Generics
import           System.IO                (Handle)

data Command = NewGame Int Int             -- ^Starts a game with given number of human players and robots
             | StartingGame PlayerName     -- ^Notification from player he is joining runnable game
             | JoinGame PlayerName GameId  -- ^Player joins an existing game
             | ListGames
             deriving (Show, Read, Generic)

data Result = PlayerRegistered PlayerName GameId
            | NewGameStarted GameId
            | GameStarts GameId
            | GamesList [GameDescription]
            | ErrorMessage String
            deriving (Show, Read, Generic)

instance ToJSON Result

data Connection = Cnx { hIn  :: Handle
                      , hOut :: Handle
                      } deriving (Show)

type Connections = M.Map PlayerName Connection

data ActiveGame = ActiveGame { activeGameId      :: GameId
                             , numberOfHumans    :: Int
                             , numberOfRobots    :: Int
                             , registeredHumans  :: Connections
                             , connectionThreads :: [ ThreadId ]
                             , gameThread        :: Maybe (Async Game)
                             }

data GameDescription = GameDescription { gameDescId           :: GameId
                                       , descNumberOfHumans   :: Int
                                       , descNumberOfRobots   :: Int
                                       , descRegisteredHumans :: [ PlayerName ]
                                       , descLive             :: Bool
                                       } deriving (Show,Read,Eq, Generic)

instance ToJSON GameDescription

gamesList :: ActiveGame -> GameDescription
gamesList ActiveGame{..} = GameDescription activeGameId numberOfHumans numberOfRobots (M.keys registeredHumans) (isJust gameThread)

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
