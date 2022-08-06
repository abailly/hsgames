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
            , test "can parse a position" <|
                \_ ->
                    let
                        msg =
                            "[15.0,5.0]"

                        expected =
                            ( 15, 5 )
                    in
                    Expect.equal (Ok expected) (Json.decodeString decodePos msg)
            , test "can parse play result" <|
                \_ ->
                    let
                        msg =
                            "{\"tag\":\"PlayerPlayed\",\"gameId\":\"MYELUVOT\",\"result\":{\"tag\":\"Placed\",\"unit\":\"13/5DP\",\"pos\":[15.0,5.0]}}"

                        expected =
                            PlayerPlayed "MYELUVOT" (Placed "13/5DP" ( 15, 5 ))
                    in
                    Expect.equal (Ok expected) (Json.decodeString decodeMessages msg)
            ]
        ]
