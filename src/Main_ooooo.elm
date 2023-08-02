module Main_ooooo exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Debug exposing (..)
import Json.Decode as JD
import Http

type Model
    = None
    | Loading
    | ExpositionsFailed
    | Display Expositions Fields

type Fields
    = Title
    | Abstract

type Location
    = Local
    | Web

type Msg
    = Noop
    | Load Location
    | GotExpositions (Result Http.Error Expositions)
    | Show Expositions Fields

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

        Load loc ->
            case loc of
                Local ->
                    ( Loading, loadExpositions Local )

                Web ->
                    ( Loading, loadExpositions Web )

        GotExpositions result ->
            case result of
                Err _ ->
                    ( ExpositionsFailed, Cmd.none )

                Ok exp ->
                    ( Display exp Abstract, Cmd.none )

        Show exp field->
            case field of
                Title ->
                    ( Display exp Title, Cmd.none )

                Abstract ->
                    ( Display exp Abstract, Cmd.none)


expositionsTable : Model -> Html Msg
expositionsTable model =
    case model of
        None ->
            div [] [ text "Ready." ]

        Loading ->
            div [] [ text "Loading..." ]
        
        Display exps fields->
            case fields of
                Title ->
                    div [ style "border" "2px solid black" ]
                    [ table [ style "border" "2px solid red", style "width" "200px" ]
                    (showFields exps Title)
                    , button [ onClick Noop ] [ text "Reset" ]
                    ]
                
                Abstract ->
                    div [ style "border" "2px solid black" ]
                    [ table [ style "border" "2px solid red", style "width" "200px" ]
                    (showFields exps Abstract)
                    , button [ onClick Noop ] [ text "Reset" ]
                    ]

        ExpositionsFailed ->
            div [] [ text "Failed." ]

showFields : Expositions -> Fields -> List (Html msg)
showFields exp field =
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
                exp

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
                exp



view : Model -> Html Msg
view model =
    
        div[]
        [ h1 [] [ text "display model state" ]
        , button [ onClick Noop ] [ text "None" ]
        , button [ onClick (Load Local) ] [ text "Local" ]
        , button [ onClick (Load Web)] [ text "Web" ]
        , button [ onClick (Show expositions Title) ] [ text "Title" ]
        , button [ onClick (Show expositions Abstract)] [ text "Abstract" ]
        --, h3 [] [text ("model state: " ++ (toString model))]
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


expositionParser : JD.Decoder Exposition
expositionParser =
    JD.map3 Exposition
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "abstract" JD.string)


expositionsParser : JD.Decoder (List Exposition)
expositionsParser =
    JD.list expositionParser

loadExpositions : Location -> Cmd Msg
loadExpositions loc =
    let
        url =
            case loc of
                Web ->
                    "https://www.danielepozzi.com/rc.json"

                Local ->
                    "http://localhost:4099/rc.json"

        _ =
            Debug.log url --??
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotExpositions expositionsParser
        }
