module Tests exposing (..)

import DayFive
import DayFour
import DayOne exposing (NumberOfEntries(..), findProduct)
import DayThree
import DayTwo
import Expect exposing (Expectation)
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
                Expect.equal 483 (DayTwo.countValidsAccordingToFirstPolicty Input.dayTwo)
        , test "second puzzle" <|
            \() ->
                Expect.equal 482 (DayTwo.countValidsAccordingToSecondPolicty Input.dayTwo)
        ]


dayThree : Test
dayThree =
    describe "Day Three"
        [ test "first puzzle" <|
            \() ->
                Expect.equal 257 (DayThree.firstPart Input.dayThree)
        , test "second puzzle" <|
            \() ->
                Expect.equal 1744787392 (DayThree.secondPart Input.dayThree)
        ]


dayFour : Test
dayFour =
    describe "Day Four"
        [ test "first puzzle" <|
            \() ->
                Expect.equal 204 (DayFour.countPassports Input.dayFour)
        , test "second puzzle" <|
            \() ->
                Expect.equal 179 (DayFour.countValidPassports Input.dayFour)
        ]


dayFive : Test
dayFive =
    describe "Day Five"
        [ test "first puzzle" <|
            \() ->
                Expect.equal 874 (DayFive.findBiggest Input.dayFive)
        ]
