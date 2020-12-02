module DayOneTest exposing (suite)

import DayOne
import DayOneInput
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Day One, first challenge"
        [ test "finds the multiplication of the matching numbers" <|
            \() ->
                Expect.equal (Just 918339) (DayOne.findProduct DayOneInput.input)
        ]
