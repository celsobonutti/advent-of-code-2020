module DayTwo exposing (countValids)


type alias Validation =
    { letter : Char
    , min : Int
    , max : Int
    , password : String
    }


countValids : String -> Int
countValids =
    parse
        >> List.filter isValid
        >> List.length


parse : String -> List Validation
parse input =
    String.split "\n" input
        |> List.filterMap parseLine


parseLine : String -> Maybe Validation
parseLine line =
    let
        splittedLine =
            line |> String.filter (\char -> char /= ':') |> String.split " "
    in
    case splittedLine of
        rawLimits :: letterString :: password :: [] ->
            case ( parseLimits rawLimits, String.uncons letterString ) of
                ( Just ( min, max ), Just ( letter, _ ) ) ->
                    Just
                        { letter = letter
                        , min = min
                        , max = max
                        , password = password
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


parseLimits : String -> Maybe ( Int, Int )
parseLimits limits =
    case
        String.split "-" limits
            |> List.map String.toInt
    of
        [ Just min, Just max ] ->
            Just ( min, max )

        _ ->
            Nothing


isValid : Validation -> Bool
isValid validation =
    let
        numberOfLetters =
            validation.password
                |> String.filter (\char -> char == validation.letter)
                |> String.length
    in
    numberOfLetters >= validation.min && numberOfLetters <= validation.max
