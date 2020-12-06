module DaySix exposing (count, countEveryone)

import Set exposing (Set)


countEveryone : String -> Int
countEveryone =
    String.split "\n\n"
        >> List.map
            (String.words
                >> checkIfEveryoneAnswered
            )
        >> List.foldl (+) 0


checkIfEveryoneAnswered : List String -> Int
checkIfEveryoneAnswered group =
    let
        numberOfPeople =
            List.length group

        everyAnswer =
            String.concat group

        possibleAnswers =
            everyAnswer
                |> String.toList
                |> Set.fromList
                |> Set.map String.fromChar
    in
    possibleAnswers
        |> Set.filter
            (\answer ->
                let
                    frequency =
                        String.indexes answer everyAnswer
                            |> List.length
                in
                frequency == numberOfPeople
            )
        |> Set.size


count : String -> Int
count =
    String.split "\n\n"
        >> List.map
            (String.filter Char.isAlpha
                >> String.toList
                >> Set.fromList
                >> Set.size
            )
        >> List.foldl (+) 0
