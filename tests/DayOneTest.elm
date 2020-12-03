module DayOneTest exposing (suite)

import DayOne exposing (NumberOfEntries(..), findProduct)
import DayOneInput
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Day One, first challenge"
        [ test "finds two entries" <|
            \() ->
                Expect.equal (Just 918339)
                    (DayOne.findProduct Two DayOneInput.input)
        , test
            "finds three entries"
          <|
            \() ->
                Expect.equal (Just 23869440) (DayOne.findProduct Three DayOneInput.input)
        ]
