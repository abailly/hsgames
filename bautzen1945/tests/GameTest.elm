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
                            ( 5, 15 )
                    in
                    Expect.equal (Ok expected) (Json.decodeString decodePos msg)
            , test "can parse play result" <|
                \_ ->
                    let
                        msg =
                            "{\"tag\":\"PlayerPlayed\",\"gameId\":\"MYELUVOT\",\"result\":{\"tag\":\"Placed\",\"unit\":\"13/5DP\",\"pos\":[15.0,5.0]}}"

                        expected =
                            PlayerPlayed "MYELUVOT" (Placed "13/5DP" ( 5, 15 ))
                    in
                    Expect.equal (Ok expected) (Json.decodeString decodeMessages msg)
            , test "can parse segment changed" <|
                \_ ->
                    let
                        msg =
                            "{\"tag\":\"PlayerPlayed\",\"gameId\":\"YRFSXFRT\",\"result\":{\"tag\":\"SegmentChanged\",\"from\":\"Setup\",\"to\":\"Supply\"}}"

                        expected =
                            PlayerPlayed "YRFSXFRT" (SegmentChanged Setup Supply)
                    in
                    Expect.equal (Ok expected) (Json.decodeString decodeMessages msg)
            ]
        ]
