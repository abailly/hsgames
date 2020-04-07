module GameServer.Utils where

import Data.Text (Text)

-- | Helper to build URIs
(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b
