{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Acquire.Net                    (InOut (..), listGames,
                                                 runNewGame, runPlayer,
                                                 runServer)
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Async       (async)
import           Control.Concurrent.Chan.Unagi  (InChan, OutChan, newChan,
                                                 readChan, writeChan)
import           Control.Exception              (SomeException, catch)
import           Control.Monad                  (forever)
import           Data.Aeson
import qualified Data.ByteString.Lazy           as BS
import           Data.Functor
import           Data.IORef
import           Data.Monoid
import           Data.Text.Lazy                 (Text, pack, unpack)
import           Data.Text.Lazy.Encoding        (decodeUtf8)
import           GHC.Generics
import           Messages
import           Network.HTTP.Types.Status
import           Network.Socket
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Handler.WebSockets as WaiWS
import           Network.Wai.Middleware.Static
import           Network.WebSockets             (Connection,
                                                 ConnectionException,
                                                 DataMessage (..),
                                                 PendingConnection,
                                                 acceptRequest,
                                                 defaultConnectionOptions,
                                                 receive, receiveDataMessage,
                                                 sendBinaryData, sendClose,
                                                 sendTextData)

newtype CommandError = CommandError { reason :: String }
  deriving (Generic)

instance ToJSON CommandError

handleWS :: Socket -> PendingConnection -> IO ()
handleWS serverSocket pending = do
  p <- socketPort serverSocket
  connection <- acceptRequest pending
  chans <- newChan
  ref   <- newIORef chans
  handleClient ref p connection

handleClient :: IORef (InChan String, OutChan String) -> PortNumber -> Connection ->  IO ()
handleClient channels p connection =
  (do
      Text message <- receiveDataMessage connection
      putStrLn $ "message: " ++ show message
      case eitherDecode message of
        Left e  -> sendTextData connection (encode $ CommandError e)
        Right c -> handleCommand c
      handleClient channels p connection)
    `catch` (\ (e :: ConnectionException) -> putStrLn $ "error: " ++ (show e))
    where
      input             = readChan
      output       chan = writeChan chan . encode
      outputResult chan = writeChan chan . encode

      io (w,r) = InOut (input r) (output w) (outputResult w)

      startGame p playerName gameId = do
        (w,r)   <- newChan
        (w',r') <- newChan
        void $ async $ runPlayer "localhost" p playerName gameId (io (w,r'))
        void $ async $ forever $ do
          v <- readChan r
          sendTextData connection v
        modifyIORef channels ( \ (w'',r'') -> (w', r''))

      handleCommand List = do
          r <- listGames "localhost" p
          putStrLn $ "result:  " ++ show r
          sendTextData connection (encode r)
      handleCommand (NewGame numHumans numRobots) = do
          r <- runNewGame "localhost" p numHumans numRobots
          sendTextData connection (encode r)
      handleCommand (JoinGame playerName gameId) = do
        startGame p playerName gameId
      handleCommand (Action n) = do
        (w, _) <- readIORef channels
        writeChan w (show n)
      handleCommand Bye = sendClose connection ("Bye!" :: Text)

main :: IO ()
main = do
  (s,_) <- runServer 0
  void $ run 9090 (WaiWS.websocketsOr defaultConnectionOptions (handleWS s) serveUI)
    where
      serveUI :: Application
      serveUI = staticPolicy (noDots >-> addBase "ui") $ defaultResponse

      defaultResponse :: Application
      defaultResponse _ respond = respond $ responseLBS status404 [] ""
