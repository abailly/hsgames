{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad.Prompt
import           Control.Monad.Reader
import           Data.List               (isPrefixOf)
import qualified Data.Map                as M
import           Interpreter
import           Net
import           Options.Applicative
import           Player
import           Pretty
import           System.Environment
import           System.IO
import           System.Random

data Configuration = Server { serverPort :: PortNumber
                            }
                   | ClientPlayer { serverHost :: String
                                  , serverPort :: PortNumber
                                  , playerName :: PlayerName
                                  , playGameId :: GameId
                                  , playerType :: PlayerType
                                  }
                   | ClientNewGame { serverHost           :: String
                                   , serverPort           :: PortNumber
                                   , numberOfHumanPlayers :: Int
                                   , numberOfRobotPlayers :: Int
                                   } deriving (Show)

configOptions :: Parser Configuration
configOptions = subparser
                ( command "server" (info serverOptions
                                    (progDesc "run an Acquire server instance listening for clients on a given port. Game starts when 6 players are connected."))
                  <> ( command "player" (info clientPlayerOptions
                                         (progDesc "run an Acquire client to connect to a given server and play as Human")))
                  <> ( command "newGame" (info newGameOptions
                                          (progDesc "connect to a server and request to start a fresh new game, returns id of the game"))))

portOption hlp = option (str >>= return . fromIntegral . read)
                 ( long "port"
                   <> short 'p'
                   <> value 7890
                   <> metavar "PORT"
                   <> help hlp)

serverOptions :: Parser Configuration
serverOptions = Server <$> portOption "Port to listen for client connections"

clientPlayerOptions :: Parser Configuration
clientPlayerOptions = ClientPlayer
                <$> strOption ( long "host"
                                <> short 'h'
                                <> value "localhost"
                                <> metavar "HOST"
                                <> help "Server host to connect to" )
                <*> portOption "Server port to connect to"
                <*> strOption ( long "player"
                                <> short 'n'
                                <> metavar "NAME"
                                <> help "Player name (must be unique for a game)" )
                <*> strOption ( long "game"
                                <> short 'g'
                                <> metavar "GAME ID"
                                <> help "Game id to connect to (set with 'newGame')" )
                <*> option auto ( long "player-type"
                                  <> short 't'
                                  <> value Human
                                  <> metavar "PLAYER-TYPE"
                                  <> help "Player type: Human or Robot" )

newGameOptions :: Parser Configuration
newGameOptions = ClientNewGame
                <$> strOption ( long "host"
                                <> short 'h'
                                <> value "localhost"
                                <> metavar "HOST"
                                <> help "Server host to connect to" )
                <*> portOption "Server port to connect to"
                <*> option auto ( long "num-humans"
                                <> short 'H'
                                <> metavar "NUMBER"
                                <> value 1
                                <> help "Number of human players")
                <*> option auto ( long "num-robots"
                                <> short 'R'
                                <> metavar "NUMBER"
                                <> value 5
                                <> help "Number of robot players")

start :: Configuration -> IO ()
start Server{..}        = runServer serverPort
start ClientPlayer{..}  = runPlayer serverHost serverPort playerName playGameId
start ClientNewGame{..} = runNewGame serverHost serverPort numberOfHumanPlayers numberOfRobotPlayers


main :: IO ()
main = execParser opts >>= start
  where
    opts = info (helper <*> configOptions)
      ( fullDesc
        <> progDesc "Run an Acquire game in client or server mode"
        <> header "Acquire - A Game on Investment" )
