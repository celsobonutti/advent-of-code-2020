module Cycle exposing (..)

import Array exposing (Array)


type Cycle
    = Cycle (Array Char)


fromList : List Char -> Cycle
fromList list =
    Cycle (Array.fromList list)


get : Int -> Cycle -> Char
get index (Cycle array) =
    let
        length =
            Array.length array

        arrIndex =
            modBy length index
    in
    Array.get arrIndex array
        |> Maybe.withDefault ' '
