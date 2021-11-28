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
    -- permitUnsetting: Bool
    }


init : State
init =
    { expanded = Nothing }


type alias Item msg =
    { icon : Html msg
    , key : String
    , display : String
    }


view : (State -> Maybe String -> msg) -> State -> List (Item msg) -> String -> Html msg
view constructor state lst currKey =
    let
        keys =
            L.map .key lst

        header =
            let
                ( attrs, display ) =
                    case lst |> L.filter (.key >> (==) currKey) |> L.head of
                        Just currItem ->
                            ( [ style "marginRight" "5px" ], text currItem.display )

                        Nothing ->
                            ( [ style "marginRight" "5px", style "fontStyle" "italic" ]
                            , text currKey
                            )
            in
            div
                [ -- closing here does NOT change value
                  onClick <| constructor { state | expanded = toggleOpen currKey state.expanded } Nothing
                , class "dropdown-current"
                ]
                [ span attrs [ display ]
                , if L.length keys > 0 then
                    span
                        [ class "dropdown-toggle"
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
                [ item.icon, text item.display ]

        tagger : Int -> Decoder msg
        tagger keyCode =
            case keyHandler keys state currKey keyCode of
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
            -- TODO use keyed?
            lst
                |> L.map (mkItem hovered)
                |> ul [ class "dropdown-list" ]
                |> mkFinal

        Nothing ->
            mkFinal (text "")


keyHandler : List String -> State -> String -> Int -> Result String ( State, Maybe String )
keyHandler keys state currKey keyCode =
    let
        openAtCurrent =
            -- down (like DOM select, open with current value)
            Ok ( { state | expanded = Just <| Maybe.withDefault currKey state.expanded }, Nothing )
    in
    case ( state.expanded, keyCode ) of
        ( Nothing, _ ) ->
            if L.member keyCode [ 38, 40 ] then
                -- up/down
                openAtCurrent

            else
                Err "Unused key"

        ( Just hovered, 40 ) ->
            -- down
            Ok ( { state | expanded = Just <| moveDown keys hovered }, Nothing )

        ( Just hovered, 38 ) ->
            -- up
            Ok ( { state | expanded = Just <| moveUp keys hovered }, Nothing )

        ( Just hovered, 13 ) ->
            -- enter
            Ok ( { state | expanded = Nothing }, Just hovered )

        ( Just _, 27 ) ->
            -- close without changing value
            Ok ( { state | expanded = Nothing }, Nothing )

        ( Just _, _ ) ->
            Err "Unused key"


moveUp : List String -> String -> String
moveUp lst currKey =
    case lst of
        hd :: nxt :: tl ->
            if currKey == nxt then
                hd

            else
                moveUp (nxt :: tl) currKey

        [ _ ] ->
            ""

        [] ->
            currKey


moveDown : List String -> String -> String
moveDown lst currKey =
    case lst of
        hd :: nxt :: tl ->
            if currKey == hd then
                nxt

            else
                moveDown (nxt :: tl) currKey

        [ hd ] ->
            hd

        [] ->
            currKey


toggleOpen : String -> Maybe String -> Maybe String
toggleOpen startVal expanded =
    case expanded of
        Just _ ->
            Nothing

        Nothing ->
            Just startVal


{-| we want to filter which keys are handled, so we need a custom eventHandler
-}
onKeyDown : (Int -> Decoder msg) -> Attribute msg
onKeyDown tagger =
    Events.on "keydown" (Decode.andThen tagger Events.keyCode)


downArrow : Html msg
downArrow =
    svg [ Attrs.version "1.1", Attrs.width "24", Attrs.height "24", Attrs.viewBox "0 0 24 24" ]
        [ Svg.path [ Attrs.d "M7.41,8.58L12,13.17L16.59,8.58L18,10L12,16L6,10L7.41,8.58Z" ] [] ]
