module Main exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (Html, button, div, h1, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Process exposing (Id)


init _ =
    ( None
    , Cmd.none
    )


type Model
    = None
    | Loading
    | Show Expositions Fields


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


type Fields
    = IdTitle
    | IdTitleAbstract


type Msg
    = Noop
    | Reset
    | GetFields Fields

expositionsTable : Model -> Html msg
expositionsTable model =
    case model of
        None ->
            div [] [ text "Ready." ]

        Loading ->
            div [] [ text "Loading..." ]

        Show exp fields ->
            div [] [ text "Show..." ]
            --div [] [ text "Loading..." ]
    
view : Model -> Html Msg
view model = div []
        [ h1 [] [ text "expositions" ]
        , button [ onClick (Reset) ] [ text "Reset" ]
        , button [ onClick (GetFields IdTitle) ] [ text "Title" ]
        , button [ onClick (GetFields IdTitleAbstract) ] [ text "Abstract" ]
        
        , expositionsTable model
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( None, Cmd.none )

        Reset ->
            ( None, Cmd.none )

        GetFields fields ->
            ( Loading, getFields fields )


getFields : Fields -> Cmd msg
getFields fields=

    case fields of
        
        IdTitle ->
            let
                _ =
                 Debug.log "cane"
            in
            
            Cmd.none

        IdTitleAbstract ->
            let
                _ =
                 Debug.log "cane"
            in
            
            Cmd.none


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
