module GameTest exposing (..)

import Expect
import Game exposing (..)
import Json.Decode as Json
import Test exposing (..)


suite : Test
suite =
    describe "Game"
        [ describe "Parse Messages"
            [ test "can parse CurrentGameSegment result" <|
                \_ ->
                    let
                        msg =
                            "{\"tag\":\"CurrentGameSegment\",\"turn\":5.0,\"side\":\"Allies\",\"segment\":\"Setup\"}"

                        expected =
                            CurrentGameSegment { turn = 1, side = Allies, segment = Setup }
                    in
                    Expect.equal (Ok expected) (Json.decodeString decodeMessages msg)
            ]
        ]
