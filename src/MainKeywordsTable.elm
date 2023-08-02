module MainKeywordsTable exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation
import Debug exposing (log)
import Html exposing (Html, button, div, h1, h2, h3, p, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (int)
import List exposing (length)
import String exposing (fromInt)
import Url exposing (Url)
import Dict exposing (Dict)


type alias Entry =
    { id : Int
    , title : String
    , abstract : String
    , keywords : List String
    }

type Field
    = Id
    | Title
    | Keywords

e1 : Entry
e1 =
    { id = 1
    , title = "first entry"
    , abstract = "this is my first entry"
    , keywords = [ "first entry", "database", "kw", "this is a keyword" ]
    }


e2 : Entry
e2 =
    { id = 1
    , title = "second entry"
    , abstract = "this is my second entry"
    , keywords = [ "second entry", "database", "this is another keyword" ]
    }


keywordsCount : Database -> KeywordsList
keywordsCount data =
    emptyKeywords emptyK


setState : Model -> ModelState -> Model
setState model newState =
    { model | state = newState }


stateToString : ModelState -> String
stateToString state =
    case state of
        None ->
            "None"

        Show ->
            "Show"


invertState : ModelState -> ModelState
invertState state =
    case state of
        None ->
            Show

        Show ->
            None


twoStatesButton : { a | state : ModelState } -> Html Msg
twoStatesButton model =
    case model.state of
        None ->
            button
                [ onClick (SetState (invertState model.state)) ]
                [ text (stateToString model.state) ]

        Show ->
            button
                [ onClick (SetState (invertState model.state)) ]
                [ text (stateToString model.state) ]


emptyK : Keyword
emptyK =
    { kw = "empty"
    , count = 0
    }


database : Database
database =
    [ e1, e2 ]


emptyKeywords : Keyword -> KeywordsList
emptyKeywords keyword =
    [ keyword, keyword ]


countEntries : Database -> Int
countEntries data =
    length data


collectKeywords : Database -> List (List String)
collectKeywords kwlist =
    List.map .keywords kwlist


showKeywords : List (List String) -> Html msg
showKeywords collectedKeywords =
    let
        kws =
            List.concat collectedKeywords

        _ =
            Debug.log (String.concat kws)
    in
    div []
        [ tr []
            [ th [ style "text-align" "left" ] [ text "id" ]
            , th [ style "text-align" "left" ] [ text "title" ]
            ]
        , kwToHtmlMsg collectedKeywords
        , div[](datasetToTable database)
        ]


datasetToTable : Database -> List (Html msg)
datasetToTable dataset =
        (tr []
            [ th [ style "border" "1px solid red" ] [ text "title" ]
            , th [ style "border" "1px solid red" ] [ text "keywords" ]
            ] :: (List.map entryToTableRow dataset)
        )


rowItem : String -> Html msg
rowItem id =
    div []
        [ text id ]


kwToHtmlMsg : List (List String) -> Html msg
kwToHtmlMsg collectedKeywords =
    div []
        (collectedKeywords |> List.concat |> List.map rowItem)


kwToTableRow : List String -> Html msg
kwToTableRow kws =
    tr []
        [ td [ style "border" "1px solid black" ]
            (List.map rowItem kws)
        ]


entryToTableRow : Entry -> Html msg
entryToTableRow entry =
    tr []
        [ td [ style "border" "1px solid black" ]
            [ div [] [ text entry.title ] ]
        , td [ style "border" "1px solid black" ]
            (List.map rowItem entry.keywords)
        ]


type alias Keyword =
    { kw : String
    , count : Int
    }

type ModelState
    = None
    | Show


type alias Database =
    List Entry


type alias KeywordsList =
    List Keyword


type alias Model =
    { state : ModelState
    , data : Database
    , keywords : KeywordsList
    , kwDict : Dict String Keyword
    }


type Msg
    = Noop
    | ClickLink Url -- import Browser
    | ChangeUrl UrlRequest -- import Url
    | LogString String
    | SetState ModelState


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd msg )
init _ _ _ =
    ( Model None database (emptyKeywords emptyK) Dict.empty
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    let
        collectedKeywords =
            collectKeywords database
    in
    { title = "URL handling example"
    , body =
        [ twoStatesButton model
        , p [] [ text ("dataset contains " ++ fromInt (countEntries model.data) ++ " entries") ]
        --, p [] [ text (String.concat e1.keywords) ]
        --, p [] [ text (String.concat e2.keywords) ]
        , div[](datasetToTable database)
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ChangeUrl url ->
            ( model, Cmd.none )

        ClickLink urlRequest ->
            ( model, Cmd.none )

        LogString string ->
            let
                _ =
                    Debug.log string
            in
            ( model, Cmd.none )

        SetState newstate ->
            ( setState model newstate, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        , onUrlChange = ClickLink
        , onUrlRequest = ChangeUrl
        }
