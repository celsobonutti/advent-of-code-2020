module DayOne exposing (findProduct)


findProduct : String -> Maybe Int
findProduct =
    parseInput
        >> findEntries
        >> multiply


parseInput : String -> List Int
parseInput =
    String.split "\n"
        >> List.filterMap String.toInt


findEntries : List Int -> Maybe ( Int, Int )
findEntries list =
    List.indexedMap (findSum list) list
        |> List.filterMap identity
        |> List.head


findSum : List Int -> Int -> Int -> Maybe ( Int, Int )
findSum list index element =
    let
        sums =
            List.drop (index + 1) list
                |> List.filter (\el -> el + element == 2020)
    in
    case sums of
        [] ->
            Nothing

        head :: _ ->
            Just ( element, head )


multiply : Maybe ( Int, Int ) -> Maybe Int
multiply =
    Maybe.map (\( fst, snd ) -> fst * snd)
