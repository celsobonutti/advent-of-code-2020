module DayTwo exposing (countValidsAccordingToFirstPolicty, countValidsAccordingToSecondPolicty, parse)

import Array


type alias Validation =
    { letter : Char
    , min : Int
    , max : Int
    , password : String
    }


countValidsAccordingToFirstPolicty : String -> Int
countValidsAccordingToFirstPolicty =
    parse
        >> List.filter validateFirstPolicy
        >> List.length


countValidsAccordingToSecondPolicty : String -> Int
countValidsAccordingToSecondPolicty =
    parse
        >> List.filter validateSecondPolicy
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


validateFirstPolicy : Validation -> Bool
validateFirstPolicy validation =
    let
        numberOfLetters =
            validation.password
                |> String.filter (\char -> char == validation.letter)
                |> String.length
    in
    numberOfLetters >= validation.min && numberOfLetters <= validation.max


validateSecondPolicy : Validation -> Bool
validateSecondPolicy validation =
    let
        indices =
            validation.password |> String.indices (String.fromChar validation.letter)
    in
    List.member (validation.min - 1) indices /= List.member (validation.max - 1) indices
