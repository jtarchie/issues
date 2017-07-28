module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Main exposing (..)


suite : Test
suite =
  describe "When listing issues" [
    describe "With no issues" <|
      let issues = [] in
      [ test "displays nothing" <|
        \_ ->
          Expect.equal ( [], Cmd.none) <| update (ListIssues (Ok issues)) []
      ]
  ]
