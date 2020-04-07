module GameServer.Utils where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import GHC.Generics
import System.Random (StdGen, randomRs)

newtype Id = Id { unId :: Text }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

randomId :: StdGen -> Id
randomId = Id . pack . take 8 . randomRs ('A','Z')

-- | Helper to build URIs
(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b
