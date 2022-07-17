module GameServer.Clients.IO where

import Control.Monad.Reader
import Data.Binary.Get (getInt64be, runGet)
import Data.Binary.Put (putInt64be, runPut)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import System.IO

{- | A connection to the server.
 Provides simple binary protocol over the wire:

  * `send` appends a newline character to the sent bytes
  * receive expects a newline-terminated bytestring
-}
data ServerConnection m = ServerConnection
    { send :: ByteString -> m ()
    , receive :: m ByteString
    , close :: m ()
    }

-- | Create a new `ServerConnection` to the given host and port.
connectTo :: String -> PortNumber -> IO (ServerConnection IO)
connectTo host port = do
    serverHandle <- connectSocketTo host port
    let send bs = LBS.hPut serverHandle (bs <> "\n")
        receive = LBS.fromStrict <$> BS.hGetLine serverHandle
        close = hClose serverHandle
    return $ ServerConnection{..}

connectSocketTo :: String -> PortNumber -> IO Handle
connectSocketTo host port = do
    sock <- socket AF_INET Stream defaultProtocol
    let hints = defaultHints{addrFamily = AF_INET, addrSocketType = Stream}
    server : _ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
    connect sock (addrAddress server)
    serverHandle <- socketToHandle sock ReadWriteMode
    hSetBuffering serverHandle NoBuffering
    pure serverHandle
