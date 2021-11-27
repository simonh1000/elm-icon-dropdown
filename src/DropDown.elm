module DropDown exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, style, tabindex)
import Html.Events as Events exposing (onBlur, onClick)
import Json.Decode as Decode exposing (Decoder)
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
                , classList [ ( "selected", item.key == hovered ) ]
                ]
                [ item.icon, span [ class "dropdown-title" ] [ text item.displayName ] ]

        tagger hovered idx =
            case keyHandler keys state hovered idx of
                Ok ( state_, return ) ->
                    Decode.succeed <| constructor state_ return

                Err err ->
                    Decode.fail err

        header =
            div
                [ class "dropdown-current" ]
                [ span [ style "marginRight" "5px" ] [ text val_ ]
                , if L.length keys > 0 then
                    span
                        [ onClick <| constructor { state | expanded = toggleOpen state.expanded } Nothing
                        , class "dropdown-toggle"
                        ]
                        [ downArrow ]

                  else
                    text ""
                ]

        mkFinal hovered htm =
            div
                [ class "dropdown-container"
                , onKeyDown (tagger hovered)
                , -- close without setting a new value
                  onBlur <| constructor { state | expanded = Nothing } Nothing
                , tabindex 0
                ]
                [ header
                , htm
                ]
    in
    case state.expanded of
        Just hovered ->
            lst
                |> L.map (mkItem hovered)
                |> ul [ class "dropdown-list" ]
                |> mkFinal hovered

        Nothing ->
            mkFinal "" (text "")


keyHandler : List String -> State -> String -> Int -> Result String ( State, Maybe String )
keyHandler keys state hovered idx =
    case idx of
        38 ->
            -- up
            Ok ( { state | expanded = Just <| moveUp keys hovered }, Nothing )

        40 ->
            -- down
            Ok ( { state | expanded = Just <| moveDown keys hovered }, Nothing )

        13 ->
            -- enter
            if state.expanded == Nothing then
                -- ignore when dropdown not open
                Err "Block enter when not open"

            else
                Ok ( { state | expanded = Nothing }, Just hovered )

        _ ->
            Err "Unused key"


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


{-| we want to filter which keys are handled, so we need a custom eventHandler
-}
onKeyDown : (Int -> Decoder msg) -> Attribute msg
onKeyDown tagger =
    Events.on "keydown" (Decode.andThen tagger Events.keyCode)


downArrow : Html msg
downArrow =
    svg [ Attrs.version "1.1", Attrs.width "24", Attrs.height "24", Attrs.viewBox "0 0 24 24" ]
        [ Svg.path [ Attrs.d "M7.41,8.58L12,13.17L16.59,8.58L18,10L12,16L6,10L7.41,8.58Z" ] [] ]
