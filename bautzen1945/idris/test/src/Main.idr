module Main

import Bautzen.GameUnit
import Bautzen.Games
import Bautzen.Id

import JSON
import Language.JSON
import Hedgehog

roundTrip : Eq a => FromJSON a => Cast a JSON => Show a => Gen a -> Property
roundTrip g = property $ do v <- forAll g
                            let enc = cast { to = JSON } v
                            footnote (show enc)
                            Right v === fromJSON enc

someId : Gen Id
someId = MkId <$> vect 8 alphaNum

someSide : Gen Side
someSide = element [ Axis, Allies ]

gamesEventNoRejoin : Vect 4 (Gen GamesEvent)
gamesEventNoRejoin = [
    NewGameCreated <$> someId,
    [| PlayerJoined someId someSide someId |],
    GameStarted <$> someId,
    [| PlayerJoined someId someSide someId |]
  ]

onlyGamesEventWithoutRejoin : Gen GamesEvent
onlyGamesEventWithoutRejoin = choice gamesEventNoRejoin

-- We don't recursively generate rejoined events list within a list of events
allGamesEvent : Gen GamesEvent
allGamesEvent = choice $
   gamesEventNoRejoin ++
   [
     [| PlayerReJoined someId someSide someId (list (constant 0 10) onlyGamesEventWithoutRejoin) |]
   ]

prop_gamesEvent : Property
prop_gamesEvent = roundTrip allGamesEvent

main : IO ()
main = test . pure $ MkGroup "Store" [
  ( "roundtrip of GamesEvent", prop_gamesEvent)
  ]
