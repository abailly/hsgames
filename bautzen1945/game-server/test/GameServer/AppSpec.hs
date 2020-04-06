module GameServer.AppSpec where

import Control.Concurrent.STM.TVar
import qualified Data.Aeson as A
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (status404)
import Network.Wai (Application)
import Network.Wai (responseLBS)
import Test.Hspec as H
import Test.Hspec.Wai as W
import Test.Hspec.Wai.Matcher as W

import GameServer.App
import GameServer.Log

mkGameServerApp :: IO Application
mkGameServerApp = do
  state <- newTVarIO mempty
  pure $ runApp fakeLogger state (\ _ resp -> resp $ responseLBS status404 [] "Nothing here")

spec :: Spec
spec =
  with mkGameServerApp $
  describe "GameServer Application" $ do
  it "on GET /games returns empty list given no game is started" $ do
    get "/games" `shouldRespondWith` ResponseMatcher 200 [] (W.bodyEquals $ A.encode ([] :: [Int]))
