module Game exposing (Model, Msg(..), divStyle, init, isNothing, main, update, view, viewDiv)

import Browser
import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import String exposing (fromInt)
import Tuple exposing (first)


type alias Position =
    ( Int, Int )


type alias UnitName =
    String


type Army
    = Russian
    | German


armyToString : Army -> String
armyToString army =
    case army of
        Russian ->
            "russian"

        German ->
            "german"


type alias Unit =
    { name : UnitName, army : Army, position : Position }


type alias Model =
    { units : Dict.Dict UnitName Unit
    , positions : Dict.Dict Position (List Unit)
    , dragDrop : DragDrop.Model UnitName Position
    }


type Msg
    = DragDropMsg (DragDrop.Msg UnitName Position)


germanOOB =
    [ "17-hq-recto"
    , "17-kg1-recto"
    , "17-kg1-verso"
    , "17-kg2-recto"
    , "17-kg2-verso"
    , "20pz-112-recto"
    , "20pz-112-verso"
    , "20pz-21-recto"
    , "20pz-21-verso"
    , "20pz-59-recto"
    , "20pz-59-verso"
    , "20pz-hq-recto"
    , "464-hq-recto"
    , "464-kg1-recto"
    , "464-kg1-verso"
    , "464-kg2-recto"
    , "464-kg2-verso"
    , "546-kg1-recto"
    , "546-kg1-verso"
    , "546-kg2-recto"
    , "546-kg2-verso"
    , "546vg-hq-recto"
    , "57pzk-hq-recto"
    , "615-687-recto"
    , "615-687-verso"
    , "615-hq-recto"
    , "615-kg-recto"
    , "732-arty-recto"
    , "777-arty-recto"
    , "black-cross"
    , "brgpzg-1-recto"
    , "brgpzg-1-verso"
    , "brgpzg-2-recto"
    , "brgpzg-2-verso"
    , "brgpzg-hq-recto"
    , "brgpzg-pz-recto"
    , "brgpzg-pz-verso"
    , "gdpzk-hq-recto"
    , "hg1-hq-recto"
    , "hg1-pz1-recto"
    , "hg1-pz1-verso"
    , "hg1-pzg1-recto"
    , "hg1-pzg1-verso"
    , "hg1-pzg2-recto"
    , "hg1-pzg2-verso"
    , "hg2-hq-recto"
    , "hg2-pzg3-recto"
    , "hg2-pzg3-verso"
    , "hg2-pzg4-recto"
    , "hg2-pzg4-verso"
    , "kg72-recto"
    , "kg72-verso"
    , "truck-recto"
    ]


russianOOB =
    [ "10dp-25-recto"
    , "10dp-27-recto"
    , "10dp-29-recto"
    , "10dp-hq-recto"
    , "14gd-36-recto"
    , "14gd-36-verso"
    , "14gd-38-recto"
    , "14gd-38-verso"
    , "14gd-41-recto"
    , "14gd-41-verso"
    , "14gd-hq-recto"
    , "14s-arty-recto"
    , "14s-recto"
    , "15gd-44-recto"
    , "15gd-44-verso"
    , "15gd-47-recto"
    , "15gd-47-verso"
    , "15gd-50-recto"
    , "15gd-50-verso"
    , "15gd-hq-recto"
    , "1kp-recto"
    , "1kp-verso"
    , "214-776-recto"
    , "214-780-recto"
    , "214-788-recto"
    , "214-hq-recto"
    , "254-929-recto"
    , "254-933-recto"
    , "254-936-recto"
    , "254-hq-recto"
    , "294-857-recto"
    , "294-859-recto"
    , "294-861-recto"
    , "294-hq-recto"
    , "2awp-hq-recto"
    , "4gd-12-recto"
    , "4gd-13-recto"
    , "4gd-14-recto"
    , "4gd-29-recto"
    , "4gd-3-recto"
    , "4gd-3-verso"
    , "4gtc-hq-recto"
    , "5dp-13-recto"
    , "5dp-13-verso"
    , "5dp-15-recto"
    , "5dp-15-verso"
    , "5dp-17-recto"
    , "5dp-17-verso"
    , "5dp-hq-recto"
    , "6l-arty-recto"
    , "7dp-33-recto"
    , "7dp-33-verso"
    , "7dp-35-recto"
    , "7dp-35-verso"
    , "7dp-37-recto"
    , "7dp-37-verso"
    , "7dp-hq-recto"
    , "7gd-24-recto"
    , "7gd-24-verso"
    , "7gd-25-recto"
    , "7gd-25-verso"
    , "7gd-26-recto"
    , "7gd-26-verso"
    , "7gd-57-recto"
    , "7gmc-hq-recto"
    , "7h-arty-recto"
    , "8c-arty-recto"
    , "8dp-32-recto"
    , "8dp-32-verso"
    , "8dp-34-recto"
    , "8dp-34-verso"
    , "8dp-36-recto"
    , "8dp-36-verso"
    , "8dp-hq-recto"
    , "95gd-284-recto"
    , "95gd-284-verso"
    , "95gd-287-recto"
    , "95gd-287-verso"
    , "95gd-290-recto"
    , "95gd-290-verso"
    , "95gd-hq-recto"
    , "9dp-26-recto"
    , "9dp-26-verso"
    , "9dp-28-recto"
    , "9dp-28-verso"
    , "9dp-30-recto"
    , "9dp-30-verso"
    , "9dp-hq-recto"
    , "9s-arty-recto"
    , "air-support-recto"
    , "air-support-verso"
    , "red-star"
    , "truck-recto"
    ]


russianUnits =
    List.map (\n -> { name = n, army = Russian, position = ( 100, 100 ) }) russianOOB


germanUnits =
    List.map (\n -> { name = n, army = German, position = ( 200, 200 ) }) germanOOB


init =
    { units =
        Dict.fromList <| List.map (\u -> ( u.name, u )) <| russianUnits ++ germanUnits
    , positions =
        Dict.fromList [ ( ( 100, 100 ), russianUnits ), ( ( 200, 200 ), germanUnits ) ]
    , dragDrop = DragDrop.init
    }


update msg model =
    case msg of
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop

                setPos pos u =
                    case u of
                        Nothing ->
                            Nothing

                        Just unit ->
                            Just { unit | position = pos }

                updatePositions maybeUnit newPosition positions =
                    case maybeUnit of
                        Nothing ->
                            positions

                        Just unit ->
                            let
                                remove =
                                    Maybe.map (\us -> List.filter (\u -> u.name /= unit.name) us)

                                add units =
                                    let
                                        newPos =
                                            { unit | position = newPosition }
                                    in
                                    case units of
                                        Nothing ->
                                            Just [ newPos ]

                                        Just us ->
                                            Just (newPos :: us)
                            in
                            Dict.update newPosition add <|
                                Dict.update unit.position
                                    remove
                                    positions
            in
            { model
                | dragDrop = model_
                , units =
                    case result of
                        Nothing ->
                            model.units

                        Just ( unitName, position, _ ) ->
                            Dict.update unitName (setPos position) model.units
                , positions =
                    case result of
                        Nothing ->
                            model.positions

                        Just ( unitName, position, _ ) ->
                            updatePositions (Dict.get unitName model.units) position model.positions
            }


divStyle : Position -> List (Attribute Msg)
divStyle ( x, y ) =
    let
        sine =
            sin (pi / 3)

        ( top, left ) =
            ( 15
                + (if modBy 2 y == 0 then
                    x * 132

                   else
                    x * 132 - 66
                  )
            , 67 + (truncate <| toFloat y * 132 * sine)
            )
    in
    [ style "text-align" "center"
    , style "width" "132px"
    , style "height" "132px"
    , style "position" "absolute"
    , style "top" (String.fromInt top ++ "px")
    , style "left" (String.fromInt left ++ "px")
    ]


parkingStyle army =
    case army of
        Russian ->
            [ style "text-align" "center"
            , style "width" "600px"
            , style "height" "2000px"
            , style "border" "3px solid red"
            , style "position" "absolute"
            , style "top" "10px"
            , style "left" "3000px"
            ]

        German ->
            [ style "text-align" "center"
            , style "width" "600px"
            , style "height" "2000px"
            , style "border" "3px solid green"
            , style "position" "absolute"
            , style "top" "10px"
            , style "left" "3650px"
            ]


mapStyle =
    [ style "background" "url('/assets/carte.png') no-repeat"
    , style "width" "3000px"
    , style "height" "2000px"
    ]


view model =
    let
        dropId =
            DragDrop.getDropId model.dragDrop

        droppablePosition =
            DragDrop.getDroppablePosition model.dragDrop

        pos j =
            List.map (\i -> ( j, i )) <| List.range 0 21

        positions =
            List.map (\p -> viewDiv (divStyle p) model.positions dropId droppablePosition p) <| List.concat (List.map pos <| List.range 0 12)

        russianParking =
            viewParking (parkingStyle Russian) model.positions dropId droppablePosition ( 100, 100 )

        germanParking =
            viewParking (parkingStyle German) model.positions dropId droppablePosition ( 200, 200 )
    in
    div mapStyle
        (russianParking :: positions ++ [ germanParking ])


isNothing maybe =
    case maybe of
        Just _ ->
            False

        Nothing ->
            True


highlighted dropId droppablePosition position =
    if dropId |> Maybe.map ((==) position) |> Maybe.withDefault False then
        case droppablePosition of
            Nothing ->
                []

            Just pos ->
                if pos.y < pos.height // 2 then
                    [ style "background-color" "cyan" ]

                else
                    [ style "background-color" "magenta" ]

    else
        []


viewParking style_ positions dropId droppablePosition position =
    let
        highlight =
            highlighted dropId droppablePosition position

        units =
            Maybe.withDefault [] <| Dict.get position positions

        styledFloating =
            always
                [ style "margin" "5px"
                ]
    in
    div
        (style_
            ++ highlight
            ++ DragDrop.droppable DragDropMsg position
        )
        (viewUnits units styledFloating)


viewDiv style_ positions dropId droppablePosition position =
    let
        highlight =
            highlighted dropId droppablePosition position

        units =
            Maybe.withDefault [] <| Dict.get position positions

        styleStacked index =
            let
                shift =
                    index * 20
            in
            [ style "margin" "5px"
            , style "position" "absolute"
            , style "top" (fromInt shift ++ "px")
            , style "left" (fromInt shift ++ "px")
            , style "z-index" (fromInt shift)
            ]
    in
    div
        (style_
            ++ highlight
            ++ DragDrop.droppable DragDropMsg position
        )
        (viewUnits units styleStacked)


viewUnits units styling =
    let
        mkImg unit ( otherUnits, index ) =
            ( img
                (src ("/assets/" ++ armyToString unit.army ++ "/" ++ unit.name ++ ".png")
                    :: DragDrop.draggable DragDropMsg unit.name
                    ++ styling index
                )
                []
                :: otherUnits
            , index + 1
            )
    in
    List.foldl mkImg ( [], 0 ) units |> first


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
