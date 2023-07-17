module Main101 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type Model
    = None
    | Loading
    | Show Fields

type Fields
    = IdTitle
    | IdTitleAbstract

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

view : Model -> Html msg
view model = div []
        [ h1 [] [ text "expositions" ]
        , button [ onClick (Loading) ] [ text "Title" ]
        , button [ onClick (None) ] [ text "Abstract" ]

        show model 
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