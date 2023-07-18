module MainRC exposing (main)

import Browser
import Html exposing (Html, button, div, h1, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD


type Model 
    = Ready
    | Loading
    | ExpositionsLoaded Expositions Int
    | ExpositionsFailed


init : () -> ( Model, Cmd Msg )
init _ =
    ( Ready
    , Cmd.none
    )


type Msg
    = Noop
    | GetExpositions Location
    | GotExpositions (Result Http.Error Expositions)
    | ShowExpositions Field Expositions


type alias Expositions =
    List Exposition


type alias Exposition =
    { id : Int
    , title : String
    , abstract : String
    }

fields = {n =3}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        GetExpositions loc ->
            ( Loading, loadExpositions loc )

        GotExpositions result ->
            case result of
                Err _ ->
                    ( ExpositionsFailed, Cmd.none )

                Ok expositions ->
                    ( ExpositionsLoaded expositions fields.n, Cmd.none )

        ShowExpositions field expositions ->
            case field of
                Title ->
                    ( ExpositionsLoaded expositions 2, Cmd.none )

                Abstract ->
                    ( ExpositionsLoaded expositions 3, Cmd.none )


type Location
    = Local
    | Web

type Field
    = Title
    | Abstract


loadExpositions : Location -> Cmd Msg
loadExpositions loc =
    let
        url =
            case loc of
                Web ->
                    "https://www.danielepozzi.com/rc.json"

                Local ->
                    "http://localhost:8080/rc.json"

        _ =
            Debug.log url --??
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotExpositions expositionsParser
        }


expositionParser : JD.Decoder Exposition
expositionParser =
    JD.map3 Exposition
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "abstract" JD.string)


expositionsParser : JD.Decoder (List Exposition)
expositionsParser =
    JD.list expositionParser



--get a list back. every item in list uses this decoder


view : Model -> Html Msg
view model =
    
        div[][h1 [] [ text "expositions" ]
        , button [ onClick (GetExpositions Local) ] [ text "Local" ]
        , button [ onClick (GetExpositions Web) ] [ text "Web" ]
        --, div[][button [ onClick (ShowExpositions Title) ] [ text "Title" ]]
        , expositionsTable model]
        --, button [ onClick (ShowExpositions Abstract) ] [ text "Abstract" ]
        


expositionsTable : Model -> Html msg
expositionsTable model =
    case model of
        Ready ->
            div [] [ text "Ready." ]

        Loading ->
            div [] [ text "Loading..." ]

        ExpositionsLoaded expositions number->
            div [ style "border" "2px solid black" ]
                [ table [ style "border" "2px solid red", style "width" "200px" ]
                (showFields expositions number)
                ]

        ExpositionsFailed ->
            div [] [ text "Failed." ]


showFields : Expositions -> Int -> List (Html msg)
showFields expositions number=
    let
        _=
            Debug.toString number
    in

    case number of 
        2 ->
            [ tr []
            [ th [ style "text-align" "left" ] [ text "id" ]
            , th [ style "text-align" "left" ] [ text "title" ]
            ]
            ]
                ++ List.map
                (\exposition ->
                    tr []
                        [ td [ style "border" "1px solid black" ] [ text (String.fromInt exposition.id) ]
                        , td [ style "border" "1px solid black" ] [ text exposition.title ]
                        ]
                )
                expositions

        3 ->
            [ tr []
            [ th [ style "text-align" "left" ] [ text "id" ]
            , th [ style "text-align" "left" ] [ text "title" ]
            , th [ style "text-align" "left" ] [ text "abstract" ]
            ]
            ]
                ++ List.map
                (\exposition ->
                    tr []
                        [ td [ style "border" "1px solid black" ] [ text (String.fromInt exposition.id) ]
                        , td [ style "border" "1px solid black" ] [ text exposition.title ]
                        , td [ style "border" "1px solid black" ] [ text exposition.abstract ]
                        ]
                )
                expositions

        _ ->
            [ tr []
            [ th [ style "text-align" "left" ] [ text "id" ]
            , th [ style "text-align" "left" ] [ text "title" ]
            , th [ style "text-align" "left" ] [ text "abstract" ]
            ]
            ]
                ++ List.map
                (\exposition ->
                    tr []
                        [ td [ style "border" "1px solid black" ] [ text (String.fromInt exposition.id) ]
                        , td [ style "border" "1px solid black" ] [ text exposition.title ]
                        , td [ style "border" "1px solid black" ] [ text exposition.abstract ]
                        ]
                )
                expositions


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
