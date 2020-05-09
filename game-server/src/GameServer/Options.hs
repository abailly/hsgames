module GameServer.Options (parseOptions, Options (..)) where

import Options.Applicative

data Options
  = Options
      { port :: Int,
        host :: String,
        backendDescriptor :: Maybe FilePath
      }

parseOptions = execParser gameServerOptionsParser

gameServerOptionsParser :: ParserInfo Options
gameServerOptionsParser =
  info
    (optionsParser <**> helper)
    (progDesc "Game Server")

optionsParser :: Parser Options
optionsParser =
  Options
    <$> portOption
    <*> hostOption
    <*> optional backendDescriptorOption

portOption :: Parser Int
portOption =
  option
    auto
    ( long "port"
        <> short 'p'
        <> metavar "PORT"
        <> value 8899
        <> help "port to listen to (default: 8899)"
    )

hostOption :: Parser String
hostOption =
  strOption
    ( long "host"
        <> short 'h'
        <> metavar "HOSTNAME"
        <> value "localhost"
        <> help "host to listen to (default: 'localhost')"
    )

backendDescriptorOption :: Parser FilePath
backendDescriptorOption =
  strOption
    ( long "backends"
        <> short 'B'
        <> help "JSON descriptor of backends served by this server"
    )
