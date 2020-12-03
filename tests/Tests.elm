module Tests exposing (dayOne, dayTwo)

import DayOne exposing (NumberOfEntries(..), findProduct)
import DayTwo exposing (countValids)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Input
import Test exposing (..)


dayOne : Test
dayOne =
    describe "Day One"
        [ test "first puzzle" <|
            \() ->
                Expect.equal (Just 918339)
                    (DayOne.findProduct Two Input.dayOne)
        , test
            "second puzzle"
          <|
            \() ->
                Expect.equal (Just 23869440) (DayOne.findProduct Three Input.dayOne)
        ]


dayTwo : Test
dayTwo =
    describe "Day Two"
        [ test "first puzzle" <|
            \() ->
                Expect.equal 483 (DayTwo.countValids Input.dayTwo)
        ]
