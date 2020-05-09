module GameServer.Clients.IO
  ( ServerConnection
  , connectTo, send, receive, disconnect
  ) where

import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Network.Socket
import Data.Binary.Get(runGet, getInt64be)
import Data.Binary.Put(runPut, putInt64be)
import System.IO

-- | A connection to the server.
-- Provides simple binary protocol over the wire:
--
--  * send prepends the sent `ByteString` with its length encoded
--    as a big-endian 64-bits integer
--  * receive expects the same format and returns the payload as
--    a lazy `ByteString`
--
data ServerConnection m =
  ServerConnection { serverHandle :: Handle
                   , send :: ByteString -> m ()
                   , receive :: m ByteString
                   }

-- | Create a new `ServerConnection` to the given host and port.
connectTo :: String -> PortNumber -> IO (ServerConnection IO)
connectTo host port = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints { addrFamily = AF_INET, addrSocketType = Stream }
  server:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  connect sock (addrAddress server)
  serverHandle <- socketToHandle sock ReadWriteMode
  hSetBuffering serverHandle NoBuffering
  let send bs = LBS.hPut serverHandle (bs <> "\n")

      receive = LBS.fromStrict <$> BS.hGetLine serverHandle

  return $ ServerConnection {..}

disconnect :: ServerConnection IO -> IO ()
disconnect ServerConnection{serverHandle} = hClose serverHandle
