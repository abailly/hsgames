module Game exposing (Model, Msg(..), divStyle, init, isNothing, main, update, view, viewDiv)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop


type alias Position =
    ( Int, Int )


type alias UnitName =
    String


type alias Unit =
    { name : UnitName, position : Position }


type alias Model =
    { data : Dict.Dict UnitName Unit
    , dragDrop : DragDrop.Model UnitName Position
    }


type Msg
    = DragDropMsg (DragDrop.Msg UnitName Position)


init =
    { data =
        Dict.fromList
            [ ( "10dp-25", { name = "10dp-25", position = ( 100, 100 ) } )
            , ( "10dp-27", { name = "10dp-27", position = ( 100, 100 ) } )
            , ( "10dp-29", { name = "10dp-29", position = ( 100, 100 ) } )
            , ( "10dp-hq", { name = "10dp-hq", position = ( 100, 100 ) } )
            ]
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
            in
            { model
                | dragDrop = model_
                , data =
                    case result of
                        Nothing ->
                            model.data

                        Just ( unitName, position, _ ) ->
                            Dict.update unitName (setPos position) model.data
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
    , style "border" "1px solid black"
    , style "position" "absolute"
    , style "top" (String.fromInt top ++ "px")
    , style "left" (String.fromInt left ++ "px")
    ]


parkingStyle =
    [ style "text-align" "center"
    , style "width" "600px"
    , style "height" "2000px"
    , style "border" "3px solid red"
    , style "position" "absolute"
    , style "top" "10px"
    , style "left" "2700px"
    ]


mapStyle =
    [ style "background" "url('/assets/carte.png') no-repeat"
    , style "width" "3000px"
    , style "height" "2000px"
    ]


unitStyle =
    [ style "margin" "5px"
    , style "position" "relative"
    ]


view model =
    let
        dropId =
            DragDrop.getDropId model.dragDrop

        position =
            DragDrop.getDroppablePosition model.dragDrop

        pos j =
            List.map (\i -> ( j, i )) <| List.range 0 21

        positions =
            List.map (viewDiv model.data dropId position) <| List.concat (List.map pos <| List.range 0 12)

        parking =
            viewParking model.data dropId position ( 100, 100 )
    in
    div mapStyle
        (positions ++ [ parking ])


isNothing maybe =
    case maybe of
        Just _ ->
            False

        Nothing ->
            True


viewDiv data dropId droppablePosition position =
    let
        highlight =
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

        mkImg unit =
            img (src ("/assets/russian/" ++ unit.name ++ "-recto.png") :: DragDrop.draggable DragDropMsg unit.name ++ unitStyle) []

        units =
            List.filter (\u -> u.position == position) (Dict.values data)
    in
    div
        (divStyle position
            ++ highlight
            ++ DragDrop.droppable DragDropMsg position
        )
        (List.map mkImg units)


viewParking data dropId droppablePosition position =
    let
        highlight =
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

        mkImg unit =
            img (src ("/assets/russian/" ++ unit.name ++ "-recto.png") :: DragDrop.draggable DragDropMsg unit.name ++ unitStyle) []

        units =
            List.filter (\u -> u.position == position) (Dict.values data)
    in
    div
        (parkingStyle
            ++ highlight
            ++ DragDrop.droppable DragDropMsg position
        )
        (List.map mkImg units)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
