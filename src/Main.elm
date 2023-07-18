module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Debug exposing (..)

type Model
    = None
    | Loading
    | Display Fields

type Fields
    = Title
    | Abstract

type Msg
    = Noop
    | Load
    | Show Fields

init : a -> ( Model, Cmd msg )
init _ =
    ( None
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( None, Cmd.none )

        Load ->
            ( Loading, Cmd.none )

        Show field->
            case field of
                Title ->
                    ( Display Title, Cmd.none )

                Abstract ->
                    ( Display Abstract, Cmd.none)


view : Model -> Html Msg
view model =
    
        div[]
        [ h1 [] [ text "display model state" ]
        , button [ onClick Noop ] [ text "None" ]
        , button [ onClick Load ] [ text "Load" ]
        , button [ onClick (Show Title) ] [ text "Title" ]
        , button [ onClick (Show Abstract)] [ text "Abstract" ]
        , h3 [] [text ("model state: " ++ (toString model))]
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