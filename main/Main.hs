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

data Configuration = Server { serverPort :: PortNumber }
                   | Client { serverHost :: String
                            , serverPort :: PortNumber
                            , playerName :: PlayerName
                            , playerType :: PlayerType
                            } deriving (Show)

configOptions :: Parser Configuration
configOptions = subparser
                ( command "server" (info serverOptions
                                    (progDesc "run an Acquire server instance listening for clients on a given port. Game starts when 6 players are connected."))
                  <> ( command "client" (info clientOptions
                                         (progDesc "run an Acquire client to connect to a given server and play as Human or Robot"))))

portOption hlp = option (str >>= return . fromIntegral . read)
                 ( long "port"
                   <> short 'p'
                   <> value 7890
                   <> metavar "PORT"
                   <> help hlp)

serverOptions :: Parser Configuration
serverOptions = Server <$> portOption "Port to listen for client connections"

clientOptions :: Parser Configuration
clientOptions = Client
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
                <*> option auto ( long "player-type"
                                  <> short 't'
                                  <> value Human
                                  <> metavar "PLAYER-TYPE"
                                  <> help "Player type: Human or Robot" )

start :: Configuration -> IO ()
start Server{..} = runServer serverPort
start Client{..} = runClient serverHost serverPort playerName


main :: IO ()
main = execParser opts >>= start
  where
    opts = info (helper <*> configOptions)
      ( fullDesc
        <> progDesc "Run an Acquire game in client or server mode"
        <> header "Acquire - A Game on Investment" )
