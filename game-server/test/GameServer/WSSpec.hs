{-# LANGUAGE NamedFieldPuns #-}

module GameServer.WSSpec where

--import           Control.Concurrent          (threadDelay)
import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.STM.TVar ()
import Control.Exception (bracket)
import Control.Monad (forM)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Bifunctor (second)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (status400)
import Network.Socket (PortNumber)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp as Warp ()
import Network.WebSockets as WS (
    DataMessage (Text),
    receiveDataMessage,
    runClient,
    sendClose,
    sendTextData,
 )
import System.Directory ()
import System.IO (hClose)
import System.Posix.Temp (mkstemp)
import Test.Hspec (Spec, around, describe, it, shouldBe)
import Prelude hiding (lines, readFile, writeFile)

import GameServer (startServer, stopServer)
import GameServer.App (initialState, runApp)
import GameServer.Builder ()
import GameServer.Clients.Echo (runEchoServer)
import GameServer.Clients.Messages (
    Message (Action, JoinGame),
    Result (GameStarts, InputRequired),
 )
import GameServer.Game (GameType (Acquire))
import GameServer.Log (LoggerEnv (logInfo), newLog)
import GameServer.Types (
    GameBackend (GameBackend),
    Server (Server, serverPort),
    ServerConfiguration (..),
 )
import GameServer.Utils (Id (unId), (</>))

testConfig :: PortNumber -> ServerConfiguration
testConfig backendPort =
    ServerConfiguration
        { serverPort = 0
        , backends = [GameBackend Acquire "localhost" (fromIntegral backendPort)]
        }

withServer :: ((Server, LoggerEnv IO, Async ()) -> IO c) -> IO c
withServer = bracket startServers stopServers
  where
    startServers = do
        logger <- newLog "test-server"
        (bport, bthread) <- runEchoServer logger (fmap (second encode) [(True, GameStarts "ABDC"), (False, InputRequired "ask for input")])
        srv <- startServer logger (testConfig bport)
        pure (srv, logger, bthread)

    stopServers (server, _, backendThread) = do
        stopServer server
        cancel backendThread

runTestClient :: LoggerEnv IO -> Int -> Id -> Id -> [ByteString] -> IO [ByteString]
runTestClient logger port gameId playerKey inputs =
    runClient "127.0.0.1" port (unpack $ "/games/Acquire" </> unId gameId </> "players" </> unId playerKey) client
  where
    client cnx = do
        outs <- forM inputs $ \inp -> do
            WS.sendTextData cnx inp
            Text o _ <- WS.receiveDataMessage cnx
            logInfo logger $ "client received " <> show o
            pure o
        WS.sendTextData cnx ("EOF" :: ByteString)
        WS.sendClose cnx ("" :: ByteString)
        pure outs

spec :: Spec
spec = around withServer $
    describe "Game Server WS Interface" $ do
        it "connects to game server backend when frontend connects to websocket at specific URL" $ \(Server{serverPort}, logger, _) -> do
            let inp = [JoinGame "ABDC" "EFGH", Action "message 1"]
                msgs = fmap encode inp
            res <- runTestClient logger serverPort "ABCD" "EFGH" msgs

            res `shouldBe` fmap encode [GameStarts "ABDC", InputRequired "ask for input"]
