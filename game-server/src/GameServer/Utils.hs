module GameServer.Utils where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.String (IsString)
import Data.Text (Text, pack)
import GHC.Generics ()
import Servant.API (FromHttpApiData (..))
import System.Random (RandomGen, StdGen, randomRs)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (Gen (MkGen))

newtype Id = Id {unId :: Text}
    deriving newtype (Eq, Ord, Show, Read, IsString, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance Arbitrary Id where
    arbitrary = MkGen $ \seed _ -> randomId seed

instance FromHttpApiData Id where
    parseUrlPiece = Right . Id

randomId :: RandomGen g => g -> Id
randomId = Id . pack . take 8 . randomRs ('A', 'Z')

-- | Helper to build URIs
(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b
