module Elevator exposing (DestinationFloor(..), Direction(..), Elevator(..), Floor, Id)


type Elevator
    = Elevator Id Floor Direction DestinationFloor


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
