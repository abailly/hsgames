module Test.Bautzen.Store

import Data.Either
import Bautzen.Games
import Bautzen.Store
import Data.SortedMap

import Test.Bautzen.Gen

import Hedgehog
import System

export
testStore : IO ()
testStore = do
  events <- sample (list (constant 0 20) onlyGamesEventWithoutRejoin)
  let expected = foldl Games.apply initialGames (GamesResEvent <$> events)
  fname <- sample genFileName
  store <- makeFileStore fname
  _ <- traverse (write store) (the (List GamesEvent) events)
  res <- read store
  case res of
     Left err => do
      print $ "Error reading event store: " ++ err
      exitFailure

     Right actual =>
       if actual /= expected
         then do
           print $ "Expected " ++ show expected  ++ ", found " ++ show actual
           exitFailure
         else pure ()
