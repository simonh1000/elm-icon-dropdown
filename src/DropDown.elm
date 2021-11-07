module DropDown exposing (..)

import Html exposing (..)


type alias State =
    { expanded : Bool }


view state lst val =
    let
        mkItem s =
            li [] [ text s ]
    in
    lst
        |> List.map mkItem
        |> ul []
