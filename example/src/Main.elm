module Main exposing (init, main)

import Browser
import DropDown as D
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
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
    ( { dState = D.init
      , selectedVal = ""
      }
    , Cmd.none
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = OnDropDownClick D.State (Maybe String)
    | OnSelect String


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

        OnSelect string ->
            ( { model | selectedVal = string }, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


options : List ( String, String )
options =
    [ ( "", "Select...." )
    , ( "alpha", "Alpha" )
    , ( "beta", "Beta" )
    , ( "gamma", "Gamma" )
    ]


view : Model -> Html Msg
view model =
    let
        mkItem ( key, display ) =
            { key = key, display = display, icon = matIcon "plus" }

        items =
            L.map mkItem options
    in
    div [ class "container p-2" ]
        [ h1 [ class "text-2xl font-bold ml-2" ] [ text "My Form" ]
        , div [ class "flex flex-col" ]
            [ formWrapper "name" <| input [] []
            , formWrapper "state (my dropdown)" <| D.view OnDropDownClick model.dState items model.selectedVal
            , formWrapper "city" <| input [] []
            , formWrapper "State (browser dropdown)" <|
                select [ onInput OnSelect ] <|
                    L.map
                        (\item -> option [ value item.key, selected (item.key == model.selectedVal) ] [ text item.display ])
                        items
            ]
        , div [ class "flex flex-col" ]
            [ div [] [ text <| "You chose " ++ model.selectedVal ] ]
        ]


formWrapper : String -> Html msg -> Html msg
formWrapper title htm =
    div [ class "ml-1 flex flex-col" ]
        [ h4 [ class "font-grey" ] [ text title ]
        , htm
        ]


matIcon : String -> Html msg
matIcon title =
    span [ class <| "mdi mdi-" ++ title ] []



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
