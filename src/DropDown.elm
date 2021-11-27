module DropDown exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, style, tabindex)
import Html.Events as Events exposing (onBlur, onClick)
import Json.Decode as Decode exposing (Decoder)
import List as L
import Svg exposing (svg)
import Svg.Attributes as Attrs


type alias State =
    { expanded : Maybe String

    -- expandOnHover: Bool
    }


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

        mkItem : String -> Item msg -> Html msg
        mkItem hovered item =
            li
                [ onClick <| constructor { state | expanded = Nothing } (Just item.key)
                , classList [ ( "selected", item.key == hovered ) ]
                ]
                [ item.icon, span [ class "dropdown-title" ] [ text item.displayName ] ]

        tagger keyCode =
            case keyHandler keys state keyCode of
                Ok ( state_, return ) ->
                    Decode.succeed <| constructor state_ return

                Err err ->
                    Decode.fail err

        mkFinal htm =
            div
                [ class "dropdown-container"
                , onKeyDown tagger
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
                |> mkFinal

        Nothing ->
            mkFinal (text "")


keyHandler : List String -> State -> Int -> Result String ( State, Maybe String )
keyHandler keys state keyCode =
    case keyCode of
        38 ->
            -- up (only makes sense when menu is open)
            Ok ( { state | expanded = Maybe.map (moveUp keys) state.expanded }, Nothing )

        40 ->
            -- down
            Ok ( { state | expanded = Just <| moveDown keys <| Maybe.withDefault "" state.expanded }, Nothing )

        13 ->
            -- enter
            case state.expanded of
                Just hovered ->
                    Ok ( { state | expanded = Nothing }, Just hovered )

                Nothing ->
                    -- ignore when dropdown not open
                    Err "Block enter when not open"

        27 ->
            -- close without changing value
            Ok ( { state | expanded = Nothing }, Nothing )

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
