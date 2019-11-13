module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, a, button, div, span, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)


initial : Int -> Model
initial elevators =
    { floors = 10
    , elevators = List.map (\i -> Elevator i 1 Stationary None) <| List.range 1 elevators
    }


main =
    Browser.sandbox { init = initial 2, update = update, view = view }


type Msg
    = Call Int


type alias Building =
    { floors : Int
    , elevators : List Elevator
    }


type Direction
    = Up
    | Down
    | Stationary


type alias Floor =
    Int


type alias Id =
    Int


type DestinationFloor
    = Floor Int
    | None


type Elevator
    = Elevator Id Floor Direction DestinationFloor


type alias Model =
    Building


update : Msg -> Model -> Model
update msg model =
    case msg of
        Call floor ->
            callElevator floor model


view : Model -> Html Msg
view model =
    renderBuilding model


renderBuilding : Building -> Html Msg
renderBuilding building =
    let
        floorCount =
            building.floors

        elevators =
            building.elevators

        render =
            renderElevatorShaft floorCount
    in
    div [] <|
        List.map render elevators


renderDirection : Direction -> String
renderDirection dir =
    case dir of
        Up ->
            "^"

        Down ->
            "v"

        Stationary ->
            ""


renderElevatorShaft : Int -> Elevator -> Html Msg
renderElevatorShaft floorCount (Elevator _ elevatorFloor direction _) =
    let
        renderFloor floor =
            if floor == elevatorFloor then
                div []
                    [ text "elevator"
                    , span [] [ a [ onClick (Call floor), href "#" ] [ text "call" ], text <| renderDirection direction ]
                    ]

            else
                div []
                    [ text "empty floor"
                    , span [] [ a [ onClick (Call floor), href "#" ] [ text "call" ] ]
                    ]
    in
    span [] <|
        List.map renderFloor (List.range 1 floorCount |> List.reverse)


callElevator : Floor -> Building -> Building
callElevator floor building =
    let
        closestElevatorId =
            findClosestElevator floor building

        direction =
            findDirection closestElevatorId floor building.elevators

        newElevators =
            updateElevatorDirection building.elevators closestElevatorId direction (Floor floor)
    in
    { building | elevators = newElevators }


findClosestElevator : Floor -> Building -> Id
findClosestElevator floor { elevators } =
    let
        closest =
            closestHelper floor

        ( elevatorId, _ ) =
            List.foldl closest ( Nothing, List.length elevators ) elevators
    in
    case elevatorId of
        Nothing ->
            -1

        Just id ->
            id


closestHelper : Floor -> Elevator -> ( Maybe Id, Int ) -> ( Maybe Id, Int )
closestHelper floor (Elevator eId eFloor _ _) ( id, distance ) =
    let
        thisDistance =
            abs (eFloor - floor)
    in
    case id of
        Nothing ->
            ( Just eId, thisDistance )

        Just d ->
            if thisDistance < d then
                ( Just eId, thisDistance )

            else
                ( id, distance )


findDirection : Id -> Floor -> List Elevator -> Direction
findDirection id floor elevators =
    let
        maybeElevator =
            List.filter (\(Elevator eId _ _ _) -> eId == id) elevators |> List.head
    in
    case maybeElevator of
        Nothing ->
            Stationary

        Just (Elevator _ eFloor _ _) ->
            let
                distance =
                    eFloor - floor
            in
            if distance > 0 then
                Down

            else
                Up


updateElevatorDirection : List Elevator -> Id -> Direction -> DestinationFloor -> List Elevator
updateElevatorDirection elevators id direction destination =
    List.map
        (\(Elevator eId floor eDirection eDestination) ->
            if eId == id then
                Elevator eId floor direction destination

            else
                Elevator eId floor eDirection eDestination
        )
        elevators
