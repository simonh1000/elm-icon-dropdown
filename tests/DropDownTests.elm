module DropDownTests exposing (..)

import DropDown exposing (..)
import Expect
import Test exposing (..)


tests : Test
tests =
    describe "tests"
        [ test "down from unopened" <|
            \_ ->
                keyHandler keys init "" 40
                    |> Expect.equal (Ok ( { expanded = Just "" }, Nothing ))
        , test "move down twice" <|
            \_ ->
                keyHandler keys init "" 40
                    |> Result.andThen (\( s, _ ) -> keyHandler keys s "" 40)
                    |> Expect.equal (Ok ( { expanded = Just "alpha" }, Nothing ))
        , test "down when value already set" <|
            \_ ->
                keyHandler keys { expanded = Nothing } "beta" 40
                    |> Expect.equal (Ok ( { expanded = Just "beta" }, Nothing ))
        ]


keys =
    [ "", "alpha", "beta", "gamma" ]
