{-# LANGUAGE NamedFieldPuns #-}
module GameServer.WSSpec where

--import           Control.Concurrent          (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Exception (bracket)
import Control.Monad (forM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (unpack)
import Network.HTTP.Types (status400)
import Network.Socket (PortNumber)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp as Warp
import Network.WebSockets as WS
import Prelude hiding (lines, readFile, writeFile)
import System.Directory
import System.IO (hClose)
import System.Posix.Temp (mkstemp)
import Test.Hspec

import GameServer
import GameServer.App (initialState, runApp)
import GameServer.Builder
import GameServer.Clients.Echo
import GameServer.Game
import GameServer.Log
import GameServer.Types
import GameServer.Utils

testConfig :: PortNumber -> ServerConfiguration
testConfig backendPort =
  ServerConfiguration { serverPort = 0
                      , backends = [ GameBackend Acquire "localhost" (fromIntegral backendPort) ] }

withServer :: ((Server, Async ()) -> IO c) -> IO c
withServer = bracket startServers stopServers
  where
    startServers = do
      (bport, bthread) <- runEchoServer
      srv <- startServer (testConfig bport)
      pure (srv, bthread)

    stopServers (server, backendThread) = do
      stopServer server
      cancel backendThread

runTestClient :: Int -> Id -> Id -> [ByteString] -> IO [ByteString]
runTestClient port gameId playerKey inputs =
  runClient "127.0.0.1" port (unpack $ "/games/Acquire" </> unId gameId </> "players" </> unId playerKey) client
  where
    client cnx = do
      outs <- forM inputs $ \ inp -> do
        WS.sendTextData cnx inp
        Text o _ <- WS.receiveDataMessage cnx
        pure o
      WS.sendTextData cnx ("EOF"::ByteString)
      WS.sendClose cnx (""::ByteString)
      pure outs

spec :: Spec
spec = around withServer $ describe "Game Server WS Interface" $ do

  it "connects to game server backend when frontend connects to websocket at specific URL" $ \ (Server{serverPort}, _) -> do
    let inp = [ "{\"tag\":\"ListGames\"}" , "message 2" ]
    res <- runTestClient serverPort "ABCD" "EFGH" inp

    res `shouldBe` inp
