module GameServer.Protocol.Tester

import Network.Socket
import Network.Socket.Data

import GameServer.Protocol.Options
import GameServer.Protocol.Messages

runTester : (sock : Socket) -> IO (Either String ())
runTester sock = do
  (buf, len) <- prepareMessage ListGames
  Right res <- sendBuf sock buf len
    | Left err => do sock_free buf ; close sock ; pure (Left $ "failed to send message to server: " ++ show err)
  if res < 0
    then do sock_free buf ; close sock ; pure (Left $ "Error code sending message: " ++ show res)
    else do sock_free buf ; close sock ; pure (Right ())

client : Int -> String -> IO (Either String ())
client port host =
  do Right sock <- socket AF_INET Stream 0
           | Left fail => pure (Left $ "Failed to open socket: " ++ show fail)
     res <- connect sock (Hostname host) port
     if res /= 0
       then pure (Left $ "Failed to connect socket with error: " ++ show res)
       else runTester sock

export
tester : Options -> IO (Either String ())
tester (MkOptions port host TesterMode) = client port host
tester _ = pure $ Left "invalid mode for mock tester"
