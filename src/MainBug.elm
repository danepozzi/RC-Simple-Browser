module MainBug exposing (..)

import Browser
import Html exposing (Html, button, div, h1, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type Model
    = None
    | Loading
    | Show Fields

type Fields
    = IdTitle
    | IdTitleAbstract

type Msg
    = Noop
    | Load

init : a -> ( Model, Cmd msg )
init _ =
    ( None
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Load ->
            ( Loading, Cmd.none )


view : Model -> Html Msg
view model =
    
        div[]
        [ h1 [] [ text "expositions" ]
        , button [ onClick Load ] [ text "Load" ]
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none