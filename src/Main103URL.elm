module Main103URL exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String exposing (filter)
import Url exposing (Url)
import Url.Parser as URL exposing (..)
import Debug exposing (log)


type alias DocsRoute =
    ( String, Maybe String )


docsParser : Parser (DocsRoute -> a) a
docsParser =
    URL.map Tuple.pair (string </> fragment identity)


type alias Model =
    { navKey : Nav.Key
    , route : Maybe DocsRoute
    , data : Expositions
    }


type Fields
    = Title
    | Abstract


type alias Exposition =
    { id : Int
    , title : String
    , abstract : String
    }


type alias Expositions =
    List Exposition


checkbox : msg -> String -> Html msg
checkbox msg name =
    label
        [ style "padding" "20px" ]
        [ input [ type_ "checkbox", onClick msg ] []
        , text name
        ]


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


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest
    | BoxChecked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUrl url ->
            ( { model | route = URL.parse docsParser url }, Cmd.none )

        ClickLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.navKey <| Url.toString url )

                External url ->
                    ( model, Nav.load url )

        BoxChecked ->
            let
                _ =
                    Debug.log "checked"
            in
            ( model, Cmd.none )


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    ( { navKey = navKey
      , route = URL.parse docsParser url
      , data = expositions
      }
    , Cmd.none
    )


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


showBody : String -> Html msg
showBody title =
    h2 [] [ text title ]


view : Model -> Document Msg
view model =
    let
        inline =
            style "display" "inline-block"

        padded =
            style "padding" "10px"

        menu =
            div [ style "padding" "10px", style "border-bottom" "1px solid #c0c0c0" ]
                [ a [ inline, padded, href "/None" ] [ text "None" ]
                , a [ inline, padded, href "/Example" ] [ text "Example" ]
                , a [ inline, padded, href "/Catalogue" ] [ text "Catalogue" ]

                -- , a [ inline, padded, href "/List#map" ] [ text "List.map" ]
                --, a [ inline, padded, href "/List#filter" ] [ text "List.filter" ]
                ]

        filter =
            div [ style "padding" "10px", style "border-bottom" "1px solid #c0c0c0" ]
                [ a [ inline, padded, href "/Id" ] [ text "Id" ]
                , a [ inline, padded, href "/Title" ] [ text "Title" ]
                , a [ inline, padded, href "/Abstract" ] [ text "Abstract" ]

                -- , a [ inline, padded, href "/List#map" ] [ text "List.map" ]
                --, a [ inline, padded, href "/List#filter" ] [ text "List.filter" ]
                ]

        title =
            case model.route of
                Just route ->
                    Tuple.first route
                        ++ (case Tuple.second route of
                                Just function ->
                                    "." ++ function

                                Nothing ->
                                    ""
                           )

                Nothing ->
                    "Invalid route"
    in
    { title = "URL handling example"
    , body =
        [ menu
        , filter
        , showBody title
        , checkbox BoxChecked "Email Notifications"
        ]
            ++ showFields expositions Title
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickLink
        , onUrlChange = ChangeUrl
        }
