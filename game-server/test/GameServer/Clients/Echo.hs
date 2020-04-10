module GameServer.Clients.Echo where

import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad (forM, forever)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import System.IO (BufferMode(NoBuffering), IOMode(..), hSetBuffering)

import GameServer.Clients.IO

runEchoServer :: IO (PortNumber, Async ())
runEchoServer = do
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
    forever $ do
      -- read bytes
      len <- runGet getInt64be <$> LBS.hGet h 8
      bs <- LBS.hGet h (fromIntegral len)
      -- echo bytes
      LBS.hPut h (runPut $ putInt64be len)
      LBS.hPut h bs

  return (port, srvThread)


echoServer = bracket startEcho stopEcho
  where
    startEcho = do
      (port, server) <- runEchoServer
      cnx <- connectTo "localhost" port
      pure (cnx, server)

    stopEcho (cnx, server) = do
      disconnect cnx
      cancel server
