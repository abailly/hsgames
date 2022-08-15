module Main

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

gamesEvent : Gen GamesEvent
gamesEvent = choice [
    NewGameCreated <$> someId
  ]

prop_gamesEvent : Property
prop_gamesEvent = roundTrip gamesEvent

main : IO ()
main = test . pure $ MkGroup "Store" [
  ( "roundtrip of GamesEvent", prop_gamesEvent)
  ]
