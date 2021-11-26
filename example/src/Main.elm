module Main exposing (init, main)

import Browser
import DropDown as D
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as L



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { dState : D.State
    , selectedVal : String
    }


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { dState = D.init, selectedVal = "" }, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = OnDropDownClick D.State (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnDropDownClick dState mbVal ->
            ( { model
                | dState = dState
                , selectedVal = mbVal |> Maybe.withDefault model.selectedVal
              }
            , Cmd.none
            )



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view model =
    let
        mkItem s =
            { icon = text "*", key = s, displayName = s }

        items =
            L.map mkItem [ "alpha", "beta", "gamma" ]
    in
    div [ class "container p-2" ]
        [ header [ class "grid-cols-3" ]
            [ h1 [ class "text-2xl font-bold ml-2" ] [ text "My Form" ]
            ]
        , D.view OnDropDownClick model.dState items model.selectedVal
        , div [ class "flex flex-col" ]
            [ div [] [ text <| "You chose " ++ model.selectedVal ] ]
        ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
