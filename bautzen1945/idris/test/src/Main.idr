module Main

import Test.Bautzen.Gen
import Test.Bautzen.Store

import JSON
import JSON.Parser
import Hedgehog

roundTrip : Eq a => FromJSON a => ToJSON a => Show a => Gen a -> Property
roundTrip g = property $ do v <- forAll g
                            let enc : JSON
                                enc = toJSON v
                            footnote (show enc)
                            Right v === fromJSON enc

prop_gamesEvent : Property
prop_gamesEvent = roundTrip allGamesEvent

prop_cost : Property
prop_cost = roundTrip genCost

main : IO ()
main = do
  Prelude.print "Checking Store"
  testStore
  test . pure $ MkGroup "Store" [
    ( "roundtrip of GamesEvent", prop_gamesEvent),
    ( "roundtrip of Cost", prop_cost)
    ]
