||| Generators for game's types
module Test.Bautzen.Gen

import public Bautzen.GameUnit
import public Bautzen.Games
import public Bautzen.Id
import public Bautzen.REPL.JSON
import public Bautzen.Terrain

import Hedgehog

export
someId : Gen Id
someId = MkId <$> vect 8 alphaNum

export
someSide : Gen Side
someSide = element [ Axis, Allies ]

export
gamesEventNoRejoin : Vect 4 (Gen GamesEvent)
gamesEventNoRejoin = [
    NewGameCreated <$> someId,
    [| PlayerJoined someId someSide someId |],
    GameStarted <$> someId,
    [| PlayerLeft someId someSide someId |]
  ]

export
onlyGamesEventWithoutRejoin : Gen GamesEvent
onlyGamesEventWithoutRejoin = choice gamesEventNoRejoin

-- We don't recursively generate rejoined events list within a list of events
export
allGamesEvent : Gen GamesEvent
allGamesEvent = choice $
   gamesEventNoRejoin ++
   [
     [| PlayerReJoined someId someSide someId (list (constant 0 10) onlyGamesEventWithoutRejoin) |]
   ]

export
genCost : Gen Cost
genCost = choice [
   pure Impossible,
   pure Zero,
   Half <$> genCost,
   One <$> genCost,
   Two <$> genCost
 ]

export
genFileName : Gen String
genFileName = pack <$> list (constant 1 20) printableAscii

export
genTerrain : Gen Terrain
genTerrain =
  choice [genSimpleTerrain, genCompositeTerrain]
  where
    genSimpleTerrain : Gen Terrain
    genSimpleTerrain = element [Clear, Wood, Rough, RoughWood, Town]

    genCompositeTerrain : Gen Terrain
    genCompositeTerrain = choice [ Hill <$> genSimpleTerrain
                                 , Village <$> genSimpleTerrain
                                 , SupplySource <$> someSide <*> genSimpleTerrain
                                 ]

partial
export
genMap : Gen Map
genMap =
  MkMap <$> sequence [ (p,) <$> genTerrain | c <- [ 0 .. 22 ]
                                           , r <- [ 0 .. 12 ]
                                           , let Right p = makePos c r]
        <*> pure []
