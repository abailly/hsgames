{-# LANGUAGE TypeApplications #-}
{-| An HTTP Game server.

The purpose of the server is twofold:

* To provide an HTTP-based facade to "simpler" TCP-based server(s) in Idris
  so that we don't have to reimplement a full-blown HTTP and WS server in
  Idris

* To manage games lifecycle and players registration logic

-}
module GameServer where

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Data.Aeson (Value, decode, object, (.=))
import Data.ByteString.Lazy (fromStrict)
import Data.Default
import Data.Text (Text)
import GameServer.App
import GameServer.Log
import GameServer.Types
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import System.Log.FastLogger (fromLogStr)
import System.Random

-- |Starts HTTP server on given port.
--
-- This server serves static files from @public/@ directory, API calls
-- from @api/@  endpoint and also
-- exposes a WebSocket-based REPL under `/repl` path.
startServer :: Int -> IO Server
startServer serverPort = do
  seed <- getStdGen
  envs <- newTVarIO (initialState seed)
  logger <- newLog "minilang"
  loggerMiddleware <- runHTTPLog logger
  let app = loggerMiddleware $ runApp logger envs staticResources
  (port, action) <- startWarp serverPort app
  thread <- async action
  logInfo logger $ object [ "action" .= ("Started" :: Text), "port" .= port ]
  pure $ Server (Just thread) port
  where
    startWarp 0 app = do
      (port, socket) <- openFreePort
      pure (port , Warp.runSettingsSocket defaultSettings socket app)
    startWarp port app = pure (port, Warp.run port app)

    runHTTPLog logger = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON
                                              , destination = Callback (\ str -> logInfo logger (decode @Value $ fromStrict $ fromLogStr str))
                                              }


stopServer :: Server -> IO ()
stopServer (Server (Just th) _) = cancel th
stopServer _                    = pure ()

waitServer :: Server -> IO ()
waitServer (Server (Just th) _) = wait th
waitServer _                    = pure ()

-- | Serve static resources under `public/` directory
staticResources :: Application
staticResources = staticApp (defaultFileServerSettings "public")
