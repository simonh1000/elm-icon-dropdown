module DropDownTests exposing (..)

import DropDown exposing (..)
import Expect
import Test exposing (..)


tests : Test
tests =
    describe "tests"
        [ test "down from unopened" <|
            \_ ->
                keyHandler keys init 40
                    |> Expect.equal (Ok ( { expanded = Just "alpha" }, Nothing ))
        , test "move down twice" <|
            \_ ->
                keyHandler keys init 40
                    |> Result.andThen (\( s, _ ) -> keyHandler keys s 40)
                    |> Expect.equal (Ok ( { expanded = Just "beta" }, Nothing ))
        , test "move down when value already set" <|
            \_ ->
                keyHandler keys { expanded = Just "alpha" } 40
                    |> Expect.equal (Ok ( { expanded = Just "beta" }, Nothing ))
        ]


keys =
    [ "alpha", "beta", "gamma" ]
