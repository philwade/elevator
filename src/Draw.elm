module Draw exposing (building, floor, shaft)

import Elevator exposing (..)
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)


buildingWidth =
    800 + 64


buildingHeight =
    800 + 64


downArrow =
    Svg.path [ d "M14.496 5.975l-.001 14.287-6.366-6.367L6 16.021l10.003 10.004L26 16.029 23.871 13.9l-6.366 6.368V5.977z" ] []


upArrow =
    Svg.path [ d "M17.504 26.025l.001-14.287 6.366 6.367L26 15.979 15.997 5.975 6 15.971 8.129 18.1l6.366-6.368v14.291z" ] []


building : Int -> List Elevator -> Html.Html msg
building floorCount elevators =
    let
        shaftWidth =
            buildingWidth // List.length elevators

        render =
            shaft floorCount shaftWidth
    in
    svg
        [ width <| String.fromInt buildingWidth
        , height <| String.fromInt buildingHeight
        , fill "white"
        ]
    <|
        List.concat <|
            List.indexedMap render elevators


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
    in
    if thisFloor == elevatorFloor then
        renderElevator xOffset yOffset floorWidth floorHeight

    else
        renderEmptyFloor xOffset yOffset floorWidth floorHeight


renderElevator : Int -> Int -> Int -> Int -> List (Html.Html msg)
renderElevator xOffset yOffset w h =
    [ rect
        [ width <| String.fromInt w
        , height <| String.fromInt h
        , x <| String.fromInt xOffset
        , y <| String.fromInt yOffset
        , fill "black"
        ]
        []
    ]


renderEmptyFloor : Int -> Int -> Int -> Int -> List (Html.Html msg)
renderEmptyFloor xOffset yOffset w h =
    let
        quarter =
            w // 4
    in
    [ rect
        [ width <| String.fromInt quarter
        , height <| String.fromInt h
        , fill "red"
        , x <| String.fromInt xOffset
        , y <| String.fromInt yOffset
        ]
        []
    , rect
        [ width <| String.fromInt <| quarter + quarter
        , height <| String.fromInt h
        , x <| String.fromInt (xOffset + quarter)
        , y <| String.fromInt yOffset
        ]
        []
    , rect
        [ width <| String.fromInt quarter
        , height <| String.fromInt h
        , fill "red"
        , x <| String.fromInt (xOffset + (quarter * 3))
        , y <| String.fromInt yOffset
        ]
        []
    ]
