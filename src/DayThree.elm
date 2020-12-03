module DayThree exposing (firstPart, secondPart)

import Cycle exposing (Cycle)


type alias Slope =
    { right : Int
    , down : Int
    }


partTwoSlopes : List Slope
partTwoSlopes =
    [ { right = 1, down = 1 }
    , { right = 3, down = 1 }
    , { right = 5, down = 1 }
    , { right = 7, down = 1 }
    , { right = 1, down = 2 }
    ]


firstPart : String -> Int
firstPart =
    parse
        >> countTrees { right = 3, down = 1 } 0 0


secondPart : String -> Int
secondPart input =
    let
        map =
            parse input
    in
    List.foldl (\slope accumulator -> accumulator * countTrees slope 0 0 map) 1 partTwoSlopes


parse : String -> List Cycle
parse =
    String.split "\n"
        >> List.map (\row -> row |> String.toList |> Cycle.fromList)


countTrees : Slope -> Int -> Int -> List Cycle -> Int
countTrees slope currentX currentCount map =
    let
        newX =
            currentX + slope.right
    in
    case List.drop slope.down map of
        [] ->
            currentCount

        head :: tail ->
            case Cycle.get newX head of
                '#' ->
                    countTrees slope newX (currentCount + 1) (head :: tail)

                _ ->
                    countTrees slope newX currentCount (head :: tail)
