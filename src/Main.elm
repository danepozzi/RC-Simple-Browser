module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

init : () -> ( Model, Cmd Msg )
init _ =
    ( None
    , Cmd.none
    )
type Model
    = None
    | Loading
    | Show Expositions Fields

type alias Expositions = List Exposition
expositions : Expositions
expositions = [ex1, ex2]

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

type Fields
    = IdTitle
    | IdTitleAbstract
type Msg
    = Noop
    | GetFields Fields

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "expositions" ]
        , button [ onClick (GetFields IdTitle) ] [ text "Title" ]
        , button [ onClick (GetFields IdTitleAbstract) ] [ text "Abstract" ]
        --, expositionsTable model
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( None, Cmd.none )
        
        GetFields fields ->
            ( Loading, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
