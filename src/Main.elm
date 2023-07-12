module Main exposing (main)

import Browser
import Html exposing (Html, div, text, h1, button, table, td, tr, th)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Html.Attributes exposing (..)


type Model =
    Ready
    | Loading
    | ExpositionsLoaded Expositions
    | ExpositionsFailed

init : () -> (Model, Cmd Msg)
init _ =
    ( Ready
    , Cmd.none
    )

type Msg
    = Noop
    | GetExpositions
    | GetExpositionsLocal
    | GotExpositions (Result Http.Error Expositions)

type alias Expositions = List Exposition

type alias Exposition = 
    { id: Int
    , title : String
    , abstract : String
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop ->
            (model, Cmd.none)

        GetExpositionsLocal ->
            (Loading, loadExpositions "local")

        GetExpositions ->
            (Loading, loadExpositions "web")

        GotExpositions result ->
            case result of
                Err _ ->
                    (ExpositionsFailed, Cmd.none)
                Ok expositions -> 
                    (ExpositionsLoaded expositions, Cmd.none)

loadExpositions which =
    if which == "local" then
        Http.get { 
        url = "http://localhost:8080/rc.json" -- http-server --cors
        , expect = Http.expectJson GotExpositions expositionsParser
        }
    else
        Http.get { 
        url = "https://www.danielepozzi.com/rc.json"
        , expect = Http.expectJson GotExpositions expositionsParser
        }

expositionParser = 
    JD.map3 Exposition
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "abstract" JD.string)

expositionsParser =
    JD.list expositionParser 
    --get a list back. every item in list uses this decoder

view : Model -> Html Msg
view model =
    div[][
        h1 [][text "expositions"]
        , button [onClick GetExpositionsLocal][text "Local"]
        , button [onClick GetExpositions][text "Web"]
        , expositionsTable model
    ]

expositionsTable model =
    case model of
        Ready ->
            div [][text "Ready."]

        Loading ->
            div [][text "Loading..."]
        
        ExpositionsLoaded expositions ->
            div [style "border" "2px solid black"][
                table [style "border" "2px solid red", style "width" "200px"] (([
                tr [][
                    th [style "text-align" "left"][text "id"]
                    , th [style "text-align" "left"][text "title"]
                    , th [style "text-align" "left"][text "abstract"]
                ]
            ]) ++ (List.map
                    (\exposition ->
                        tr [][
                        td [style "border" "1px solid black"][text (String.fromInt exposition.id)]
                        , td [style "border" "1px solid black"][text exposition.title]
                        , td [style "border" "1px solid black"][text exposition.abstract]
                        ]
                    )
                    expositions))
            ]

        ExpositionsFailed ->
            div [][text "Failed."]

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