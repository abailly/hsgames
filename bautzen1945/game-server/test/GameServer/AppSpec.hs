module GameServer.AppSpec where

import Control.Concurrent.STM.TVar
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (status404)
import Network.Wai (Application)
import Network.Wai (responseLBS)
import Network.Wai.Test as W
import System.Random
import Test.Hspec as H
import Test.Hspec.Wai as W
import Test.Hspec.Wai.Matcher as W

import GameServer.App
import GameServer.Builder
import GameServer.Log
import GameServer.Player
import GameServer.Utils

mkGameServerApp :: IO Application
mkGameServerApp = do
  state <- newTVarIO $ initialState testSeed
  pure $ runApp fakeLogger state (\ _ resp -> resp $ responseLBS status404 [] "Nothing here")

spec :: Spec
spec =
  with mkGameServerApp $
  describe "GameServer Application" $ do
  describe "Games" $ do

    it "on GET /games returns empty list given no game is started" $ do
      get "/games" `shouldRespondWith` ResponseMatcher 200 [] (W.bodyEquals $ A.encode ([] :: [Int]))

    it "on GET /games returns game given one game is started" $ do
      let game = Game "mygame"
      postJSON "/games" game

      get "/games" `shouldRespondWith` ResponseMatcher 200 [] (W.bodyEquals $ A.encode [game])

    it "on POST /games returns 201 given input game is valid" $ do
      postJSON "/games" (Game "mygame")
        `shouldRespondWith` 201

    it "on POST /games returns game's id in Location header given input game is valid" $ do
      postJSON "/games" (Game "mygame")
        `shouldRespondWith` ResponseMatcher 201 [("Location" <:> encodeUtf8 ("/games" </> unId (randomId testSeed))) ] ""

    it "on POST /games ensures returned game id is unique" $ do
      location <- fromJust . lookup "Location" . W.simpleHeaders <$> postJSON "/games" (Game "mygame")

      postJSON "/games" (Game "othergame")
        `shouldRespondWith` ResponseMatcher 201 (locationDifferentFrom location) ""

    it "on POST /games returns 400 given game name already exists" $ do
      postJSON "/games" (Game "mygame")

      postJSON "/games" (Game "mygame")
        `shouldRespondWith` 400

  describe "Players" $ do

    it "on POST /players returns 201 given input player is valid" $ do
      postJSON "/players" aPlayer
        `shouldRespondWith` ResponseMatcher 201 [("Location" <:> encodeUtf8 ("/players" </> (playerName aPlayer))) ] ""

    it "on GET /players returns list of registered players" $ do
      postJSON "/players" aPlayer

      get "/players"
        `shouldRespondWith` ResponseMatcher 200 [] (W.bodyEquals $ A.encode [aPlayer])




locationDifferentFrom :: ByteString -> [MatchHeader]
locationDifferentFrom loc =
  [ W.MatchHeader $
    \ headers _ -> case lookup "Location" headers of
                     Nothing -> Just "No Location header"
                     Just h | h == loc -> Just ("expected 'Location' header to differ from " <> show loc)
                     Just h -> Nothing
  ]
