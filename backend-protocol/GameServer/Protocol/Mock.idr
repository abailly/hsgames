module GameServer.Protocol.Mock

import Data.Buffer
import Data.List
import Network.Socket
import Network.Socket.Data
import Network.Socket.Raw
import System

import GameServer.Protocol.Options
import GameServer.Protocol.Messages
import GameServer.Protocol.Binary

%cg chez libidris_net.so

handleClient : Socket -> SocketAddress -> (len_buf : BufPtr) -> IO ()
handleClient socket addr len_buf = do
  putStrLn $ "waiting for input"
  Right res <- recvBuf socket len_buf 8 -- receive 8 bytes representing length of message
    | Left err => do putStrLn ("failed to read length of message " ++ show err) ; close socket
  if res < 0
    then do putStrLn ("error reading message length: " ++ show res) ; close socket
    else do len <- read64be len_buf
            data_buf <- sock_alloc (cast len)
            Right res <- recvBuf socket data_buf (fromInteger len)
              | Left err => do putStrLn ("failed to read message of len " ++ show len ++ ": " ++ show err) ; sock_free data_buf ; close socket
            if res < 0
              then  do putStrLn ("failed to read message " ++ show res) ; sock_free data_buf ; close socket
              else do
                Right msg <- decodeMessage data_buf len
                  | Left err => do putStrLn ("failed to decode message " ++ show err) ; sock_free data_buf
                print msg
                sock_free data_buf
                handleClient socket addr len_buf


serve : Socket -> IO (Either String ())
serve sock = do
  Right (s, addr) <- accept sock
    | Left err => pure (Left $ "Failed to accept on socket with error: " ++ show err)
  pid <- fork $ do
    len_buf <- sock_alloc 8
    handleClient s addr len_buf
  serve sock

export
server : Int -> String -> IO (Either String ())
server port host = do
  Right sock <- socket AF_INET Stream 0
        | Left fail => pure (Left $ "Failed to open socket: " ++ show fail)
  res <- bind sock (Just (Hostname host)) port
  if res /= 0
    then pure (Left $ "Failed to bind socket with error: " ++ show res)
    else do
      res <- listen sock
      if res /= 0
        then pure (Left $ "Failed to listen on socket with error: " ++ show res)
        else serve sock

export
mock : Options -> IO (Either String ())
mock (MkOptions port host MockMode) = server port host
mock _ = pure $ Left "invalid mode for mock server"
