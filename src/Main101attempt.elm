module Main101attempt exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (Html, button, div, h1, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Process exposing (Id)

init : a -> ( Model, Cmd msg )
init _ =
    ( None
    , Cmd.none
    )
type Model
    = None
    | Loading
    | Show Fields

type Fields
    = IdTitle
    | IdTitleAbstract

update : Model -> b -> ( Model, Cmd msg )
update msg model =
    case msg of
        None ->
            ( None, Cmd.none )

        Loading ->
            ( Loading, Cmd.none )

        Show fields ->
            ( Show fields, Cmd.none )

show : Model -> Fields -> Html msg
show model fields=
    case model of
        None ->
            div [] [ text "Ready." ]

        Loading ->
            div [] [ text "Loading..." ]

        Show field->
            case fields of
                IdTitle ->
                    div [] [ text "IdTitle." ]
                IdTitleAbstract ->
                    div [] [ text "IdTitleAbstract." ]
            --div [] [ text "Loading..." ]

view : Model -> Html Model
view model = div []
        [ h1 [] [ text "expositions" ]
        , button [ onClick (Loading) ] [ text "Title" ]
        , button [ onClick (None) ] [ text "Abstract" ]

        --, show model 
        ]


--data
type alias Expositions =
    List Exposition
expositions : Expositions
expositions =
    [ ex1, ex2 ]
type alias Exposition =
    { id : Int
    , title : String
    , abstract : String
    }
ex1 : Exposition
ex1 =
    { id = 10
    , title = "cazzo di dio"
    , abstract = "e la madonna cagna"
    }
ex2 : Exposition
ex2 =
    { id = 13
    , title = "porco di dio"
    , abstract = "e la madonna infame"
    }

subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


main : Program () Model Model
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }