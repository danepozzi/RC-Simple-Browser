module MainAppBoilerplate exposing (..)

import Json.Decode exposing (int)
import Url
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation
import Html exposing (Html, button, div, h1, h2, h3, table, td, text, th, tr)
import Html.Attributes exposing (..)
import Url exposing (Url)

type alias Entry =
    { id : Int
    , title : String
    , abstract : String
    , keywords: List String
    }

e1 : Entry
e1 =
    { id = 1
    , title = "first entry"
    , abstract = "this is my first entry"
    , keywords = ["first entry, database"]
    }

e2 : Entry
e2 =
    { id = 1
    , title = "second entry"
    , abstract = "this is my second entry"
    , keywords = ["second entry, database"]
    }

emptyK : Keyword
emptyK = 
    { kw = "empty"
    , count = 0
    }

database : Database
database = [e1, e2]

emptyKeywords : Keyword -> KeywordsList
emptyKeywords keyword= 
    [keyword, keyword]

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
    }

type Msg
    = Noop
    | ClickLink Url -- import Browser
    | ChangeUrl UrlRequest -- import Url

init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd msg )
init _ _ _=
    ( 
    Model None database (emptyKeywords emptyK)
    , Cmd.none
    )    

view : Model -> Document Msg
view model = 
    
    { title = "URL handling example"
    , body =
        [ button[][ text "boilerplate" ]]
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ChangeUrl url ->
            ( model, Cmd.none )

        ClickLink urlRequest ->
            (model, Cmd.none)

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