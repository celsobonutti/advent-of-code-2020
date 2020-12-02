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
    findInList (\el -> el + element == 2020) (List.drop (index + 1) list)
        |> Maybe.map (\sum -> ( element, sum ))


multiply : Maybe ( Int, Int ) -> Maybe Int
multiply =
    Maybe.map (\( fst, snd ) -> fst * snd)


findInList : (Int -> Bool) -> List Int -> Maybe Int
findInList fun list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if fun head then
                Just head

            else
                findInList fun tail
