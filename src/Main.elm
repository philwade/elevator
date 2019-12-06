module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, a, button, div, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Time


initial : () -> ( Model, Cmd Msg )
initial _ =
    let
        elevators =
            3
    in
    ( { floors = 10
      , elevators = List.map (\i -> Elevator i 1 Stationary None) <| List.range 1 elevators
      , queuedRequests = []
      }
    , Cmd.none
    )


main =
    Browser.element { init = initial, update = update, view = view, subscriptions = subscriptions }


type Msg
    = Call Int
    | Tick Time.Posix


type alias Building =
    { floors : Int
    , elevators : List Elevator
    , queuedRequests : List Int
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Call floor ->
            ( callElevator floor model, Cmd.none )

        Tick _ ->
            ( timeTick model, Cmd.none )


timeTick : Building -> Building
timeTick { floors, elevators, queuedRequests } =
    { floors = floors
    , elevators =
        List.map
            (\(Elevator id currentFloor direction destination) ->
                case destination of
                    Floor x ->
                        if currentFloor == x then
                            Elevator id currentFloor Stationary None

                        else if direction == Up && currentFloor < floors then
                            Elevator id (currentFloor + 1) direction destination

                        else if direction == Down && floors > 1 then
                            Elevator id (currentFloor - 1) direction destination

                        else
                            Elevator id currentFloor direction destination

                    None ->
                        Elevator id currentFloor direction destination
            )
            elevators
    , queuedRequests = queuedRequests
    }


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
    span [ class "column" ] <|
        List.map renderFloor (List.range 1 floorCount |> List.reverse)


callElevator : Floor -> Building -> Building
callElevator floor building =
    let
        closestElevatorId =
            findClosestElevator floor building
    in
    case closestElevatorId of
        Nothing ->
            { building | queuedRequests = floor :: building.queuedRequests }

        Just id ->
            let
                direction =
                    findDirection id floor building.elevators

                newElevators =
                    updateElevatorDirection building.elevators id direction (Floor floor)
            in
            { building | elevators = newElevators }


findClosestElevator : Floor -> Building -> Maybe Id
findClosestElevator floor { elevators } =
    let
        closest =
            closestHelper floor

        ( elevatorId, _ ) =
            List.foldl closest ( Nothing, List.length elevators ) elevators
    in
    elevatorId


closestHelper : Floor -> Elevator -> ( Maybe Id, Int ) -> ( Maybe Id, Int )
closestHelper floor (Elevator eId eFloor _ destination) ( id, distance ) =
    let
        thisDistance =
            abs (eFloor - floor)
    in
    if destination /= None then
        ( id, distance )

    else
        case id of
            Nothing ->
                ( Just eId, thisDistance )

            Just _ ->
                if thisDistance < distance then
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick
