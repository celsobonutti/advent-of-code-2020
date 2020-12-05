module DayFive exposing (findBiggest)

import List exposing (head)


type alias Limit =
    { upper : Int
    , lower : Int
    }


type alias Divisors =
    { upper : Char
    , lower : Char
    }


rowDivisors : Divisors
rowDivisors =
    { upper = 'B'
    , lower = 'C'
    }


columnDivisors : Divisors
columnDivisors =
    { upper = 'R'
    , lower = 'L'
    }


findBiggest : String -> Int
findBiggest =
    parse
        >> List.map calculateId
        >> List.maximum
        >> Maybe.withDefault 0


calculateId : List Char -> Int
calculateId boardingPass =
    let
        row =
            split (List.take 7 boardingPass) rowDivisors { lower = 0, upper = 127 }

        column =
            split (List.drop 7 boardingPass) columnDivisors { lower = 0, upper = 7 }
    in
    (row * 8) + column


parse : String -> List (List Char)
parse =
    String.lines
        >> List.map String.toList


calculateNewLimit : Limit -> Float
calculateNewLimit { lower, upper } =
    (toFloat lower + toFloat upper) / 2


split : List Char -> Divisors -> Limit -> Int
split input divisor limit =
    case input of
        head :: [] ->
            if head == divisor.upper then
                limit.upper

            else
                limit.lower

        head :: tail ->
            if head == divisor.upper then
                split tail divisor { limit | lower = ceiling (calculateNewLimit limit) }

            else
                split tail divisor { limit | upper = floor (calculateNewLimit limit) }

        _ ->
            0
