{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Chan.Unagi  (InChan, dupChan, newChan,
                                                 readChan, writeChan)
import           Control.Monad                  (forever)
import           Data.ByteString.Lazy           (ByteString)
import           Data.Functor
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets as WaiWS
import           Network.WebSockets             (DataMessage (..),
                                                 PendingConnection,
                                                 acceptRequest,
                                                 defaultConnectionOptions,
                                                 receiveDataMessage,
                                                 sendTextData)


handleWS :: InChan ByteString -> PendingConnection -> IO ()
handleWS bcast pending = do
    localChan <- dupChan bcast
    connection <- acceptRequest pending

    void $ forkIO $ forever $ do
        message <- readChan localChan
        sendTextData connection message

    -- loop forever
    let loop = do
            Text message <- receiveDataMessage connection
            writeChan bcast message
            loop
    loop


main :: IO ()
main = do
    (bcast, _) <- newChan
    void $ run 9090 (WaiWS.websocketsOr defaultConnectionOptions (handleWS bcast) undefined)
