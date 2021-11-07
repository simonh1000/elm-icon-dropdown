module Main exposing (init, main)

import Browser
import DropDown as D
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { counter : Int
    , serverMessage : String
    }


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { counter = flags, serverMessage = "" }, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Inc


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Inc ->
            ( add1 model, Cmd.none )


add1 : Model -> Model
add1 model =
    { model | counter = model.counter + 1 }



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view model =
    div [ class "container p-2" ]
        [ header [ class "grid-cols-3" ]
            [ h1 [ class "text-2xl font-bold ml-2" ] [ text "My Form" ]
            ]
        , D.view () [ "alpha", "beta", "gamma" ] "alpha"
        , div [ class "flex flex-col" ]
            [ h2 [] [ text "hello header 2" ] ]
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
