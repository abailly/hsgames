module GameServer.Clients.IOSpec where

import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad (forM, forever)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import System.IO (BufferMode(NoBuffering), IOMode(..), hSetBuffering)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import GameServer.Clients.Echo
import GameServer.Clients.IO

spec :: Spec
spec = describe "To-Server I/O Protocol" $ do

  around echoServer $ describe "given a compliant server" $ do

    it "can send and receive arbitrary bytestrings not containing '\\n' over the wire" $ \ (cnx, _) ->
      property $ can_send_and_receive_arbitrary_data cnx

newtype Exchange = Exchange { unExchange :: [ ByteString ] }
  deriving (Eq, Show)

instance Arbitrary Exchange where
  arbitrary = Exchange . fmap LBS.pack . fmap (filter ((/= '\n') . toEnum . fromIntegral)) <$> arbitrary

can_send_and_receive_arbitrary_data :: ServerConnection IO -> Exchange -> Property
can_send_and_receive_arbitrary_data cnx (Exchange is) = monadicIO $ do
  os <- forM is $ \ i -> run $ do
    send cnx i
    receive cnx
  assert (os == is)
