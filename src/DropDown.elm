module DropDown exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, style, tabindex)
import Html.Events as Events exposing (onClick)
import Json.Decode as Decode
import List as L
import Svg exposing (svg)
import Svg.Attributes as Attrs


type alias State =
    { expanded : Maybe String }


init : State
init =
    { expanded = Nothing }


type alias Item msg =
    { icon : Html msg
    , key : String
    , displayName : String
    }


view : (State -> Maybe String -> msg) -> State -> List (Item msg) -> String -> Html msg
view constructor state lst val =
    let
        keys =
            L.map .key lst

        val_ =
            if val == "" then
                "Select ..."

            else
                val

        mkItem : String -> Item msg -> Html msg
        mkItem hovered item =
            li
                [ onClick <| constructor { state | expanded = Nothing } (Just item.key)
                , tabindex 0
                , classList [ ( "selected", item.key == hovered ) ]
                ]
                [ item.icon, span [ class "dropdown-title" ] [ text item.displayName ] ]

        tagger hovered idx =
            case Debug.log "idx" idx of
                38 ->
                    -- up
                    constructor { state | expanded = Just <| moveUp keys hovered } Nothing

                40 ->
                    -- down
                    constructor { state | expanded = Just <| moveDown keys hovered } Nothing

                13 ->
                    -- enter
                    constructor { state | expanded = Nothing } (Just hovered)

                _ ->
                    constructor state Nothing

        header =
            div
                [ class "dropdown-current"
                ]
                [ span [ style "marginRight" "5px" ] [ text val_ ]
                , span
                    [ onClick <| constructor { state | expanded = toggleOpen state.expanded } Nothing
                    , class "dropdown-toggle"
                    ]
                    [ downArrow ]
                ]
    in
    case state.expanded of
        Just hovered ->
            div
                [ class "dropdown-container"
                , onKeyDown (tagger hovered)
                ]
                [ header
                , lst
                    |> L.map (mkItem hovered)
                    |> ul [ class "dropdown-list" ]
                ]

        Nothing ->
            div
                [ class "dropdown-container"
                , tabindex 0
                ]
                [ header ]


moveUp : List String -> String -> String
moveUp lst curr =
    case lst of
        hd :: nxt :: tl ->
            if curr == nxt then
                hd

            else
                moveUp (nxt :: tl) curr

        [ _ ] ->
            ""

        [] ->
            curr


moveDown : List String -> String -> String
moveDown lst curr =
    case lst of
        hd :: nxt :: tl ->
            if curr == "" then
                hd

            else if curr == hd then
                nxt

            else
                moveDown (nxt :: tl) curr

        [ hd ] ->
            hd

        [] ->
            curr


toggleOpen : Maybe a -> Maybe String
toggleOpen expanded =
    case expanded of
        Just _ ->
            Nothing

        Nothing ->
            Just ""


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Events.on "keydown" (Decode.map tagger Events.keyCode)


downArrow : Html msg
downArrow =
    svg [ Attrs.version "1.1", Attrs.width "24", Attrs.height "24", Attrs.viewBox "0 0 24 24" ]
        [ Svg.path [ Attrs.d "M7.41,8.58L12,13.17L16.59,8.58L18,10L12,16L6,10L7.41,8.58Z" ] [] ]
