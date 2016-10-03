{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Acquire.Net                    (InOut (..), listGames,
                                                 runNewGame, runPlayer,
                                                 runServer)
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Async       (async)
import           Control.Exception              (SomeException, catch)
import qualified Data.ByteString.Lazy           as BS
import           Data.Functor
import           Data.Monoid
import           Data.Text.Lazy                 (Text, pack, unpack)
import           Data.Text.Lazy.Encoding        (decodeUtf8)
import           Messages
import           Network.Socket
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets as WaiWS
import           Network.WebSockets             (Connection,
                                                 ConnectionException,
                                                 DataMessage (..),
                                                 PendingConnection,
                                                 acceptRequest,
                                                 defaultConnectionOptions,
                                                 receive, receiveDataMessage,
                                                 sendClose, sendTextData)

handleWS :: Socket -> PendingConnection -> IO ()
handleWS serverSocket pending = do
  p <- socketPort serverSocket
  connection <- acceptRequest pending

  handleClient p connection

handleClient :: PortNumber -> Connection ->  IO ()
handleClient p connection =
  (do
      Text message <- receiveDataMessage connection
      case read (unpack $ decodeUtf8 message) of
        List -> do
          r <- listGames "localhost" p
          sendTextData connection (pack $ show r)
        NewGame numHumans numRobots -> do
          r <- runNewGame "localhost" p numHumans numRobots
          sendTextData connection (pack $ show r)
        JoinGame playerName gameId ->
          runPlayer "localhost" p playerName gameId io
        Bye  -> sendClose connection ("Bye!" :: Text)
        _    -> pure ()
      handleClient p connection)
    `catch` (\ (e :: ConnectionException) -> putStrLn (show e))
    where
      input = do
        Text message <- receiveDataMessage connection
        return $ unpack $ decodeUtf8 message
      output = sendTextData connection . pack . show
      io = InOut input output False

main :: IO ()
main = do
  (s,_) <- runServer 0
  void $ run 9090 (WaiWS.websocketsOr defaultConnectionOptions (handleWS s) undefined)
