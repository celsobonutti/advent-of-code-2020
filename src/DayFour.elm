module DayFour exposing (countPassports, countValidPassports)

import Char
import Dict exposing (Dict)


type alias FieldDict =
    Dict String String


type alias Passport =
    { byr : String
    , iyr : String
    , eyr : String
    , hgt : String
    , hcl : String
    , ecl : String
    , pid : String
    , cid : Maybe String
    }


requiredFields : List String
requiredFields =
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"
    ]


countPassports : String -> Int
countPassports =
    splitPassports
        >> List.map splitFields
        >> List.map listToDictionary
        >> List.filterMap dictionaryToPassport
        >> List.length


countValidPassports : String -> Int
countValidPassports =
    splitPassports
        >> List.map splitFields
        >> List.map listToDictionary
        >> List.map dictionaryToPassport
        >> List.filterMap validatePassport
        >> List.length


validatePassport : Maybe Passport -> Maybe Passport
validatePassport passport =
    passport
        |> Maybe.andThen validateByr
        |> Maybe.andThen validateIyr
        |> Maybe.andThen validateEyr
        |> Maybe.andThen validateHgt
        |> Maybe.andThen validateHcl
        |> Maybe.andThen validateEcl
        |> Maybe.andThen validatePid


splitPassports : String -> List String
splitPassports =
    String.split "\n\n"


splitFields : String -> List String
splitFields =
    String.split "\n"
        >> List.concatMap (String.split " ")


listToDictionary : List String -> FieldDict
listToDictionary =
    List.foldl addToDict Dict.empty


addToDict : String -> FieldDict -> FieldDict
addToDict field currentDict =
    case String.split ":" field of
        key :: value :: [] ->
            Dict.insert key value currentDict

        _ ->
            currentDict


dictionaryToPassport : FieldDict -> Maybe Passport
dictionaryToPassport fields =
    let
        byr =
            Dict.get "byr" fields

        iyr =
            Dict.get "iyr" fields

        eyr =
            Dict.get "eyr" fields

        hgt =
            Dict.get "hgt" fields

        hcl =
            Dict.get "hcl" fields

        ecl =
            Dict.get "ecl" fields

        pid =
            Dict.get "pid" fields

        cid =
            Dict.get "cid" fields
    in
    case [ byr, iyr, eyr, hgt, hcl, ecl, pid ] of
        [ Just by, Just iy, Just ey, Just hg, Just hc, Just ec, Just pi ] ->
            Just
                { byr = by
                , iyr = iy
                , eyr = ey
                , hgt = hg
                , hcl = hc
                , ecl = ec
                , pid = pi
                , cid = cid
                }

        _ ->
            Nothing


validateYear : Int -> Int -> String -> Bool
validateYear min max yearStr =
    case String.toInt yearStr of
        Just year ->
            if year >= min && year <= max then
                True

            else
                False

        _ ->
            False


validateByr : Passport -> Maybe Passport
validateByr passport =
    if validateYear 1920 2002 passport.byr then
        Just passport

    else
        Nothing


validateIyr : Passport -> Maybe Passport
validateIyr passport =
    if validateYear 2010 2020 passport.iyr then
        Just passport

    else
        Nothing


validateEyr : Passport -> Maybe Passport
validateEyr passport =
    if validateYear 2020 2030 passport.eyr then
        Just passport

    else
        Nothing


validateHgt : Passport -> Maybe Passport
validateHgt passport =
    case ( String.toInt (String.dropRight 2 passport.hgt), String.right 2 passport.hgt ) of
        ( Just height, "cm" ) ->
            if height >= 150 && height <= 193 then
                Just passport

            else
                Nothing

        ( Just height, "in" ) ->
            if height >= 59 && height <= 76 then
                Just passport

            else
                Nothing

        _ ->
            Nothing


validateHcl : Passport -> Maybe Passport
validateHcl passport =
    case ( String.left 1 passport.hcl, String.dropLeft 1 passport.hcl ) of
        ( "#", color ) ->
            if String.length color == 6 && List.all Char.isHexDigit (String.toList color) then
                Just passport

            else
                Nothing

        _ ->
            Nothing


eyeColors : List String
eyeColors =
    [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]


validateEcl : Passport -> Maybe Passport
validateEcl passport =
    if List.member passport.ecl eyeColors then
        Just passport

    else
        Nothing


validatePid : Passport -> Maybe Passport
validatePid passport =
    if String.length passport.pid == 9 && List.all Char.isDigit (String.toList passport.pid) then
        Just passport

    else
        Nothing
