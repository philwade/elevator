module Draw exposing (building, floor, shaft)

import Elevator exposing (..)
import Html
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


buildingWidth =
    800


buildingHeight =
    800


downArrow call =
    Svg.path [ d "M 14.496 5.975 l -.001 14.287 -6.366 -6.367 L 6 16.021 l 10.003 10.004 L 26 16.029 23.871 13.9 l -6.366 6.368 V 5.977 z", onClick call ] []


upArrow call =
    Svg.path [ d "M 47.504 26.025 l .001 -14.287 6.366 6.367 L 56 15.979 45.997 5.975 36 15.971 38.129 18.1 l 6.366 -6.368 v 14.291 z", onClick call ] []


renderControls call =
    [ rect
        [ width "25"
        , height "25"
        , stroke "black"
        , strokeWidth "2"
        , fill "transparent"
        , x "4"
        , y "3"
        , onClick call
        ]
        []
    , rect
        [ width "25"
        , height "25"
        , stroke "black"
        , strokeWidth "2"
        , fill "transparent"
        , x "34"
        , y "3"
        , onClick call
        ]
        []
    , downArrow call
    , upArrow call
    ]


building : Int -> List Elevator -> (Int -> msg) -> Html.Html msg
building floorCount elevators callFunc =
    let
        shaftWidth =
            buildingWidth // List.length elevators

        render =
            shaft floorCount shaftWidth

        renderedElevators =
            List.concat <|
                List.indexedMap render elevators

        controlPanel =
            controls floorCount callFunc
    in
    Html.div [] <|
        List.concat
            [ [ svg
                    [ width <| String.fromInt buildingWidth
                    , height <| String.fromInt buildingHeight
                    , fill "white"
                    ]
                    renderedElevators
              ]
            , controlPanel
            ]


controls : Int -> (Int -> msg) -> List (Html.Html msg)
controls floorCount callFunc =
    let
        floorHeight =
            buildingHeight // floorCount

        onefloor floorNumber =
            svg
                [ width "100"
                , height <| String.fromInt floorHeight
                , Html.Attributes.style "left" <| String.fromInt buildingWidth
                , Html.Attributes.style "top" <| String.fromInt <| (buildingHeight - (floorNumber * floorHeight))
                , Html.Attributes.style "position" "absolute"
                ]
                (renderControls (callFunc floorNumber))
    in
    List.map
        onefloor
        (List.range 1 floorCount)


shaft : Int -> Int -> Int -> Elevator -> List (Html.Html msg)
shaft floorCount width offset (Elevator _ elevatorFloor direction _) =
    let
        xOffset =
            width * offset

        floorHeight =
            buildingHeight // floorCount

        renderFloor =
            floor elevatorFloor xOffset floorHeight width
    in
    List.concat <|
        List.map
            renderFloor
            (List.range 1 floorCount |> List.reverse)


floor : Int -> Int -> Int -> Int -> Int -> List (Html.Html msg)
floor elevatorFloor xOffset floorHeight floorWidth thisFloor =
    let
        yOffset =
            buildingHeight - (thisFloor * floorHeight)

        wallIncrement =
            10

        increment =
            floorWidth // wallIncrement

        secondWallFactor =
            wallIncrement - 1

        elevatorFactor =
            wallIncrement - 2

        emptyFloor =
            [ rect
                [ width <| String.fromInt increment
                , height <| String.fromInt floorHeight
                , fill "grey"
                , x <| String.fromInt xOffset
                , y <| String.fromInt yOffset
                ]
                []
            , rect
                [ width <| String.fromInt increment
                , height <| String.fromInt floorHeight
                , fill "grey"
                , x <| String.fromInt (xOffset + (increment * secondWallFactor))
                , y <| String.fromInt yOffset
                ]
                []
            ]
    in
    if thisFloor == elevatorFloor then
        List.append emptyFloor <|
            [ rect
                [ width <| String.fromInt <| increment * elevatorFactor
                , height <| String.fromInt floorHeight
                , x <| String.fromInt (xOffset + increment)
                , y <| String.fromInt yOffset
                , stroke "black"
                , strokeWidth "5"
                , fill "transparent"
                ]
                []
            ]

    else
        emptyFloor
