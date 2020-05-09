module GameServer.Clients.Echo where

import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad (forM_, forever, when)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Socket
import System.IO (BufferMode(NoBuffering), IOMode(..), hSetBuffering)

import GameServer.Clients.IO
import GameServer.Log

runEchoServer :: LoggerEnv IO -> [(Bool, ByteString)] -> IO (PortNumber, Async ())
runEchoServer logger outputs = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  let hints = defaultHints { addrFlags = [AI_PASSIVE]
                           , addrSocketType = Stream
                           , addrFamily = AF_INET
                           }
  addr:_ <- getAddrInfo (Just hints) (Just "localhost") Nothing
  withFdSocket sock $ setCloseOnExecIfNeeded
  bind sock (addrAddress addr)
  listen sock 1
  Just port <- socketPortSafe sock
  srvThread <- async $ do
    (clientSock, _) <- accept sock
    h <- socketToHandle clientSock ReadWriteMode
    hSetBuffering h NoBuffering
    handleClient h outputs

  return (port, srvThread)
    where
      handleClient h [] = forever $ do
        -- read bytes
        bs <- LBS.fromStrict <$> BS.hGetLine h
        logInfo logger $ "received payload " <> decodeUtf8 bs
          -- echo bytes
        BS.hPut h (LBS.toStrict bs <> "\n")
        logInfo logger $ "sent response " <> decodeUtf8 bs

      handleClient h outputs =
        forM_ outputs $ \ (waitInput, out) -> do
          -- read bytes
          when waitInput $ do
            bs <- LBS.fromStrict <$> BS.hGetLine h
            logInfo logger $ "received payload " <> decodeUtf8 bs

            -- echo bytes
          BS.hPut h (LBS.toStrict out <> "\n")
          logInfo logger $ "sent response " <> decodeUtf8 out


echoServer = bracket startEcho stopEcho
  where
    startEcho = do
      (port, server) <- runEchoServer fakeLogger []
      cnx <- connectTo "localhost" port
      pure (cnx, server)

    stopEcho (cnx, server) = do
      disconnect cnx
      cancel server
