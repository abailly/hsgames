module GameServer.Utils where

import Data.Aeson (FromJSONKey, ToJSONKey, FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text, pack)
import GHC.Generics
import Servant.API (FromHttpApiData(..))
import System.Random (StdGen, randomRs)

newtype Id = Id { unId :: Text }
  deriving newtype (Eq, Ord, Show, Read, IsString, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance FromHttpApiData Id where
  parseUrlPiece = Right . Id

randomId :: StdGen -> Id
randomId = Id . pack . take 8 . randomRs ('A','Z')

-- | Helper to build URIs
(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b
