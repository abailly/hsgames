{-| Various builders to build test data -}
module GameServer.Builder where

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Text (Text)
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import System.IO.Unsafe
import System.Random
import Test.Hspec.Wai as W

import GameServer.Game
import GameServer.Player

testSeed :: StdGen
testSeed = mkStdGen 42

postJSON :: (A.ToJSON a) => ByteString -> a -> WaiSession () SResponse
postJSON path payload = request "POST" path [("Content-type", "application/json")] (A.encode payload)

putJSON :: (A.ToJSON a) => ByteString -> a -> WaiSession () SResponse
putJSON path payload = request "PUT" path [("Content-type", "application/json")] (A.encode payload)

aPlayer :: Player
aPlayer = Player { playerName = "Alice" }

anotherPlayer :: Player
anotherPlayer = Player { playerName = "Bob" }

anEmptyGame :: Game
anEmptyGame = makeNewGame Bautzen1945 0

anotherEmptyGame :: Game
anotherEmptyGame = makeNewGame Acquire 0
