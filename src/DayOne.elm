module DayOne exposing (NumberOfEntries(..), findProduct)


type NumberOfEntries
    = Two
    | Three


findProduct : NumberOfEntries -> String -> Maybe Int
findProduct numberOfEntries =
    parseInput
        >> findMultiples numberOfEntries


parseInput : String -> List Int
parseInput =
    String.split "\n"
        >> List.filterMap String.toInt


findMultiples : NumberOfEntries -> List Int -> Maybe Int
findMultiples numberOfEntries list =
    case numberOfEntries of
        Two ->
            findTwoEntries list
                |> multiplyTwo

        Three ->
            findThreeEntries list
                |> multiplyThree


findTwoEntries : List Int -> Maybe ( Int, Int )
findTwoEntries list =
    List.indexedMap (findTwo list) list
        |> List.filterMap identity
        |> List.head


findTwo : List Int -> Int -> Int -> Maybe ( Int, Int )
findTwo list index element =
    let
        subsequentElements =
            List.drop (index + 1) list
    in
    findInList (\el -> el + element == 2020) subsequentElements
        |> Maybe.map (\el -> ( element, el ))


findThreeEntries : List Int -> Maybe ( Int, Int, Int )
findThreeEntries list =
    List.indexedMap (findThree list) list
        |> List.filterMap identity
        |> List.head


findThree : List Int -> Int -> Int -> Maybe ( Int, Int, Int )
findThree list index element =
    let
        subsequentElements =
            List.drop (index + 1) list
    in
    subsequentElements
        |> List.indexedMap
            (\sndIndex snd ->
                findInList (\thr -> element + snd + thr == 2020) subsequentElements
                    |> Maybe.map (\thr -> ( snd, thr ))
            )
        |> List.filterMap identity
        |> List.head
        |> Maybe.map
            (\( snd, thr ) ->
                ( element, snd, thr )
            )


multiplyTwo : Maybe ( Int, Int ) -> Maybe Int
multiplyTwo =
    Maybe.map (\( fst, snd ) -> fst * snd)


multiplyThree : Maybe ( Int, Int, Int ) -> Maybe Int
multiplyThree =
    Maybe.map (\( fst, snd, thd ) -> fst * snd * thd)


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
