module Example exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Json
import Json.Encode as Enc
import Messages exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "JSON Conversions"
        [ let
            gamesListMsg =
                "{\"tag\":\"GamesList\",\"contents\":[{\"descNumberOfRobots\":5,\"descNumberOfHumans\":1,\"descLive\":false,\"gameDescId\":\"MLFXFNXE\",\"descRegisteredHumans\":[]}]}"

            gamesListObj =
                GamesList
                    [ { gameDescId = "MLFXFNXE"
                      , descNumberOfHumans = 1
                      , descNumberOfRobots = 5
                      , descRegisteredHumans = []
                      , descLive = False
                      }
                    ]
          in
          describe "GamesList"
            [ test "can decode GamesList message" <|
                \_ ->
                    Expect.equal (Ok gamesListObj) (Json.decodeString decodeMessages gamesListMsg)
            ]
        , test "can decode cell content" <|
            \_ ->
                let
                    json =
                        "[[{\"row\":\"A\",\"col\":1},{\"cellContent\":{\"tag\":\"Empty\"},\"cellCoord\":{\"row\":\"A\",\"col\":1}}]]"

                    expectedBoard =
                        Dict.fromList [ ( ( 'A', 1 ), { cellCoord = ( 'A', 1 ), cellContent = Empty } ) ]
                in
                Expect.equal (Ok expectedBoard) (Json.decodeString decodeBoard json)
        ]
