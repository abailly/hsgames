{-# LANGUAGE TypeApplications #-}

module GameServer.Clients.IOSpec where

import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Lens (ix, (^?))
import Control.Monad (forM, forever)
import Data.Aeson (Value, eitherDecode)
import Data.Aeson.Lens (key, _Array)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import GameServer.Backend.Bautzen1945 (withBautzenServer)
import GameServer.Clients.Echo
import GameServer.Clients.IO (ServerConnection (..))
import Network.Socket
import System.IO (BufferMode (NoBuffering), IOMode (..), hSetBuffering)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = describe "To-Server I/O Protocol" $ do
    around echoServer $
        describe "given a compliant server" $
            it "can send and receive arbitrary bytestrings not containing '\\n' over the wire" $ \(cnx, _) ->
                property $ can_send_and_receive_arbitrary_data cnx

    around withBautzenServer $
        describe "Bautzen 1945 server" $
            it "can send and receive a sequence of commands" $ \ServerConnection{send, receive, close} -> do
                send "12345678"
                receive `shouldReturn` "OK"
                send "[\"positions?\"]"
                r <- eitherDecode @Value <$> receive
                case r of
                    Left e -> fail $ show e
                    Right v ->
                        v `shouldSatisfy` \v ->
                            (v ^? _Array . ix 0 . key "unit" . key "nation") == Just "Polish"
                close

newtype Exchange = Exchange {unExchange :: [ByteString]}
    deriving (Eq, Show)

instance Arbitrary Exchange where
    arbitrary = Exchange . fmap LBS.pack . fmap (filter ((/= '\n') . toEnum . fromIntegral)) <$> arbitrary

can_send_and_receive_arbitrary_data :: ServerConnection IO -> Exchange -> Property
can_send_and_receive_arbitrary_data cnx (Exchange is) = monadicIO $ do
    os <- forM is $ \i -> run $ do
        send cnx i
        receive cnx
    assert (os == is)
