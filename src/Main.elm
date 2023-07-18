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

type alias Exposition =
    { id : Int
    , title : String
    , abstract : String
    }

type alias Expositions =
    List Exposition

ex1 : Exposition
ex1 = 
    { id = 1
    , title = "mannaggia"
    , abstract = "cristoforo colombo"
    }

ex2 : Exposition
ex2 = 
    { id = 2
    , title = "santo"
    , abstract = "antonio cristo colombo"
    }

expositions : Expositions
expositions =
    [ ex1, ex2 ]

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


expositionsTable : Model -> Html Msg
expositionsTable model =
    case model of
        None ->
            div [] [ text "Ready." ]

        Loading ->
            div [] [ text "Loading..." ]
        
        Display fields->
            case fields of
                Title ->
                    div [ style "border" "2px solid black" ]
                    [ table [ style "border" "2px solid red", style "width" "200px" ]
                    (showFields Title)
                    ]
                
                Abstract ->
                    div [ style "border" "2px solid black" ]
                    [ table [ style "border" "2px solid red", style "width" "200px" ]
                    (showFields Abstract)
                    ]

showFields :Fields -> List (Html msg)
showFields field =
    case field of
        Title ->
            tr []
            [ th [ style "text-align" "left" ] [ text "id" ]
            , th [ style "text-align" "left" ] [ text "title" ]
            ]
                :: List.map
                (\exposition ->
                    tr []
                        [ td [ style "border" "1px solid black" ] [ text (String.fromInt exposition.id) ]
                        , td [ style "border" "1px solid black" ] [ text exposition.title ]
                        ]
                )
                expositions

        Abstract ->
            tr []
            [ th [ style "text-align" "left" ] [ text "id" ]
            , th [ style "text-align" "left" ] [ text "title" ]
            , th [ style "text-align" "left" ] [ text "abstract" ]
            ]
                :: List.map
                (\exposition ->
                    tr []
                        [ td [ style "border" "1px solid black" ] [ text (String.fromInt exposition.id) ]
                        , td [ style "border" "1px solid black" ] [ text exposition.title ]
                        , td [ style "border" "1px solid black" ] [ text exposition.abstract ]
                        ]
                )
                expositions



view : Model -> Html Msg
view model =
    
        div[]
        [ h1 [] [ text "display model state" ]
        , button [ onClick Noop ] [ text "None" ]
        , button [ onClick Load ] [ text "Load" ]
        , button [ onClick (Show Title) ] [ text "Title" ]
        , button [ onClick (Show Abstract)] [ text "Abstract" ]
        , h3 [] [text ("model state: " ++ (toString model))]
        , h1 [] [text ("table")]
        , expositionsTable model
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