{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Acquire.Net                    (listGames, runServer)
import           Data.Functor
import           Data.Text.Lazy                 (pack, unpack)
import           Data.Text.Lazy.Encoding        (decodeUtf8)
import           Messages
import           Network.Socket
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets as WaiWS
import           Network.WebSockets             (DataMessage (..),
                                                 PendingConnection,
                                                 acceptRequest,
                                                 defaultConnectionOptions,
                                                 receiveDataMessage,
                                                 sendTextData)

handleWS :: Socket -> PendingConnection -> IO ()
handleWS serverSocket pending = do
  p <- socketPort serverSocket
  connection <- acceptRequest pending

  Text message <- receiveDataMessage connection

  case read (unpack $ decodeUtf8 message) of
    List -> do
      r <- listGames "localhost" p
      sendTextData connection (pack $ show r)
    _    -> return ()


main :: IO ()
main = do
  (s,_) <- runServer 0
  void $ run 9090 (WaiWS.websocketsOr defaultConnectionOptions (handleWS s) undefined)
