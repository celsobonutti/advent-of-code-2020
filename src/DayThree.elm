module DayThree exposing (..)

import Cycle exposing (Cycle)


count : String -> Int
count =
    parse
        >> countTrees 0 0


parse : String -> List Cycle
parse =
    String.split "\n"
        >> List.map (\row -> row |> String.toList |> Cycle.fromList)


countTrees : Int -> Int -> List Cycle -> Int
countTrees currentX currentCount map =
    let
        newX =
            currentX + 3
    in
    case List.drop 1 map of
        [] ->
            currentCount

        head :: tail ->
            case Cycle.get newX head of
                '#' ->
                    countTrees newX (currentCount + 1) (head :: tail)

                _ ->
                    countTrees newX currentCount (head :: tail)
