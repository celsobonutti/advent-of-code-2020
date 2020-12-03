module Tests exposing (dayOne, dayThree, dayTwo)

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
