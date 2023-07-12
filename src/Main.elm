module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Main00buttons exposing (Msg)


type alias Model = { }

init : () -> (Model, Cmd Msg)
init _ =
    ( Model
    , Cmd.none
    )

type Msg
    = Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

view : Model -> Html Msg
view model =
    div[][text "text"]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }