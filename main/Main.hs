{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad.Prompt
import           Control.Monad.Reader
import qualified Data.Map                as M
import           Interpreter
import           Network.Socket
import           Options.Applicative
import           Player
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
start Server{..} = startServer serverPort
start Client{..} = runClient serverHost serverPort playerName

startServer :: PortNumber -> IO ()
startServer port = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 5
  clients <- waitForClients sock 2 [] >>= mapM getHandle
  close sock
  g <- getStdGen
  let connections = M.fromList $ ("Console", Cnx stdin stdout) : map (\ (p,h) -> (p, Cnx h h)) clients
  runReaderT (runPromptM playerInputHandler $ initialisedGame g (map (\ (p,_) -> (p,Human)) clients) >>= interpretCommand) connections
    where
      getHandle (player,sock) = socketToHandle sock ReadWriteMode >>= return . (player,)


waitForClients :: Socket -> Int -> [(PlayerName, Socket)] -> IO [(PlayerName, Socket)]
waitForClients _    0 clients = return clients
waitForClients sock n clients = do
  (clientSock, clientAddr) <- accept sock
  putStrLn $ "client connection from " ++ show clientAddr
  (playerName, _, _) <- recvFrom clientSock 64
  putStrLn $ "connecting player " ++ show playerName
  waitForClients sock (n-1) ((playerName,clientSock):clients)

runClient :: String -> PortNumber -> PlayerName -> IO ()
runClient host port player = do
  sock <- socket AF_INET Stream defaultProtocol
  server:_ <- getAddrInfo Nothing (Just host) (Just $ show port)
  connect sock  (addrAddress server)
  len <- send sock player
  putStrLn $ "registering " ++ player ++ " with server at address " ++ show (addrAddress server)
  playClient sock

playClient :: Socket -> IO ()
playClient sock = do
  void $ forkIO $ receiveData
  sendData
    where
      receiveData = do
        (game, _, _) <- recvFrom sock 4096
        putStrLn game
        receiveData
      sendData = do
        line <- getLine
        void $ send sock (line ++ "\n")
        sendData

main :: IO ()
main = execParser opts >>= start
  where
    opts = info (helper <*> configOptions)
      ( fullDesc
        <> progDesc "Run an Acquire game in client or server mode"
        <> header "Acquire - A Game on Investment" )

  -- let players = map read playersDescription
