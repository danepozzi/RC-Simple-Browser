module Main exposing (..)

import Browser
import Debug exposing (..)
import Html exposing (Html, input, button, div, h1, h2, h3, table, td, text, th, tr, a)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Dict exposing (Dict)
import Set exposing (Set)


type alias Model =
    { state : ModelState
    , expositions : Expositions
    , browseBy : BrowseBy
    , portals : Set String
    , journals : Set String
    , authors : Set String
    , portal : String
    , journal : String
    , author : String
    , keyword : String
    , fieldsToShow : Set String
    }


type ModelState
    = None
    | Loading
    | ExpositionsFailed
    | Display Expositions BrowseBy


type BrowseBy
    = ByPortal
    | ByJournal
    | ByAuthor
    | ByKeyword


type Location
    = Local
    | Web


type Msg
    = Noop
    | Load Location
    | GotExpositions (Result Http.Error Expositions)
    | Show Expositions BrowseBy
    | LoadPortals
    | SetPortal String
    | SetJournal String
    | SetAuthor String
    | SetKeyword String
    | SetField String
    | ClickedOnKeyword String

type alias Exposition =
    { abstract : String
    , defaultPage : String
    , id : Int
    , keywords : List String
    , publication : List Publication
    , author : Author
    --, thumb : String
    , title : String
    --, type_ : String
    }


type alias Expositions =
    List Exposition


ex1 : Exposition
ex1 =
    { author = {id = 1, name = "santantonoi"}
    , abstract = "santantonio e la madonna"
    , defaultPage = "https://www.researchcatalogue.net/view/2081778/2081726"
    , id = 2081778
    , keywords = ["Artistic research","Sustainability","Media and culture","Creative process","Performing arts","Film and media","insights","embodiment of knowledge","collaborations","Nordic region","Interdisciplinary expressions","ecology of artistic research","sustainability","artistic research","Media","knowledge systems","performing arts","Film studies","Embodied knowledge","collaboration","nordic region","interdisciplinarity","ecology of practices","ecology of creativity"]
    , publication = [{id = 2, name ="Journal for Artistic Research"}]
    --, thumb = "https://media.researchcatalogue.net/rc/cache/70/aa/bb/a3/70aabba38a294253242ed2ccc290f3a8.png?t=007838df6bffc82f44653b92c23cdbd6&e=1689127200"
    , title = "The Ecology of Artistic Research"
    --, type_ = "exposition"
    }


ex2 : Exposition
ex2 =
    { author = {id = 1, name = "santantonoi"}
    , abstract = "santantonio e la madonna"
    , defaultPage = "https://www.researchcatalogue.net/view/2081778/2081726"
    , id = 2081778
    , keywords = ["Artistic research","Sustainability","Media and culture","Creative process","Performing arts","Film and media","insights","embodiment of knowledge","collaborations","Nordic region","Interdisciplinary expressions","ecology of artistic research","sustainability","artistic research","Media","knowledge systems","performing arts","Film studies","Embodied knowledge","collaboration","nordic region","interdisciplinarity","ecology of practices","ecology of creativity"]
    , publication = [{id = 2, name ="Journal for Artistic Research"}]
    --, thumb = "https://media.researchcatalogue.net/rc/cache/70/aa/bb/a3/70aabba38a294253242ed2ccc290f3a8.png?t=007838df6bffc82f44653b92c23cdbd6&e=1689127200"
    , title = "The Ecology of Artistic Research"
    --, type_ = "exposition"
    }


expositions : Expositions
expositions =
    [ ex1, ex2 ]

-- () remove flag parsing
init : () -> ( Model, Cmd msg )
init _ =
    ( Model 
        None 
        expositions 
        ByPortal 
        Set.empty
        Set.empty
        Set.empty  
        "all" 
        "all"
        "all"
        "" 
        Set.empty
    , Cmd.none
    )

expositionsLoaded : Expositions -> Model -> ( Model, Cmd Msg )
expositionsLoaded exps model =
    ( { model | expositions = exps, state = Display exps ByPortal }, Cmd.none )

getPublication : Exposition -> String
getPublication exp =
    let
        maybePub = List.head exp.publication
    in
   
    case maybePub of
        Nothing ->
            "not published"

        Just publication ->
            publication.name

getAuthor : Exposition -> String
getAuthor exp =
    let
        author = exp.author
    in
   
    author.name

addAuthor : Exposition -> Model -> Model
addAuthor exp model = 
    let
        author = getAuthor exp
    in
    { model | authors = Set.insert author model.portals}

addPortal : Exposition -> Model -> Model
addPortal exp model = 
    let
        portal = getPublication exp
    in
   
    { model | portals = Set.insert portal model.portals} 


getPortal : Exposition -> String
getPortal exp =
    let
        maybePub = List.head exp.publication
    in

    case maybePub of
        Nothing ->
            "not published"

        Just publication ->
            if List.member publication.name portals then
                publication.name
            else
                "other"

portals = ["KC Research Portal", "Stockholm University of the Arts (SKH)", "University of the Arts Helsinki", "Norwegian Academy of Music", "The Danish National School of Performing Arts", "Rhythmic Music Conservatory Copenhagen", "Konstfack - University of Arts, Crafts and Design", "NTNU", "i2ADS - Research Institute in Art, Design and Society", "University of Applied Arts Vienna", "Academy of Creative and Performing Arts", "International Center for Knowledge in the Arts (Denmark)", "Inland Norway University of Applied Sciences, The Norwegian Film School", "Fontys Academy of the Arts (internal)"]

getJournal : Exposition -> String
getJournal exp =
    let
        maybePub = List.head exp.publication
    in

    case maybePub of
        Nothing ->
            "not published"

        Just publication ->
            if List.member publication.name journals then
                publication.name
            else
                "other"

journals = ["Journal for Artistic Research", "RUUKKU - Studies in Artistic Research", "Journal of Sonic Studies", "VIS - Nordic Journal for Artistic Research", "HUB - Journal of Research in Art, Design and Society"]

addPortals : Expositions -> Model -> Model
addPortals exps model =
    let
        listPortals = List.map getPortal exps
        setPortals = Set.fromList listPortals
    in
    { model | portals = setPortals }

addJournals : Expositions -> Model -> Model
addJournals exps model =
    let
        listJournals = List.map getJournal exps
        setJournals = Set.fromList listJournals
    in
    { model | journals = setJournals }

addAuthors : Expositions -> Model -> Model
addAuthors exps model =
    let
        listAuthors = List.map getAuthor exps
        setAuthors = Set.fromList listAuthors
    in
    { model | authors = setAuthors }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( { model | state = None }, Cmd.none )

        Load loc ->
            case loc of
                Local ->
                    ( { model | state = Loading }, loadExpositions Local )

                Web ->
                    ( { model | state = Loading }, loadExpositions Web )

        GotExpositions result ->
            case result of
                Err _ ->
                    ( { model | state = ExpositionsFailed }, Cmd.none )

                Ok exp ->
                    expositionsLoaded exp model

        Show exp browseBy ->
            case browseBy of
                ByPortal ->
                    ( model
                        |> addPortals model.expositions
                        |> setModelFields ByPortal
                        |> setModelPortal "all"
                        |> (setModelState (Display model.expositions ByPortal))
                        , Cmd.none 
                    )
                    --( { model | state = Display model.expositions ByPortal }, Cmd.none )

                ByJournal ->
                    ( model
                        |> addJournals model.expositions
                        |> setModelFields ByJournal
                        |> setModelJournal "all"
                        |> (setModelState (Display model.expositions ByJournal))
                        , Cmd.none 
                    )

                ByAuthor ->
                    ( model
                        |> addAuthors model.expositions
                        |> setModelFields ByAuthor
                        |> setModelAuthor "all"
                        |> (setModelState (Display model.expositions ByAuthor))
                        , Cmd.none 
                    )

                ByKeyword ->
                    ( model
                        |> addAuthors model.expositions
                        |> setModelFields ByKeyword
                        |> (setModelState (Display model.expositions ByKeyword))
                        , Cmd.none 
                    )

        LoadPortals ->
            (addPortals model.expositions model, Cmd.none)

        SetPortal string ->
            ( { model | portal = string }, Cmd.none)

        SetJournal string ->
            ( { model | journal = string }, Cmd.none)

        SetAuthor string ->
            ( { model | author = string }, Cmd.none)

        SetKeyword string ->
            ( { model | keyword = string }, Cmd.none)

        SetField field ->
            ( updateFieldsToShow model field, Cmd.none)

        ClickedOnKeyword kw ->
            ( { model | keyword = kw, browseBy = ByKeyword, state = (Display model.expositions ByKeyword) }, Cmd.none)


viewModel : Model -> Html Msg
viewModel model =
    case model.state of
        None ->
            div [] [ text "Ready." ]

        Loading ->
            div [] [ text "Loading..." ]

        Display exps browseBy ->
            case browseBy of
                ByPortal -> 
                    viewTable model (filterByPortal model.portal exps)

                ByJournal -> 
                    viewTable model (filterByJournal model.journal exps)

                ByAuthor -> 
                    viewTable model (filterByAuthor model.author exps)

                ByKeyword -> 
                    viewTable model (filterByKeyword model.keyword exps)
            
        ExpositionsFailed ->
            div [] [ text "Failed." ]

isKwInKws : String -> Exposition -> Bool
isKwInKws str exp =
    List.member str exp.keywords

isPortalInPublication : String -> Exposition -> Bool
isPortalInPublication str exp =
    let
        maybePub = List.head exp.publication
    in
   
    case maybePub of
        Nothing ->
            False

        Just publication ->
            publication.name == str   

isJournalInPublication : String -> Exposition -> Bool
isJournalInPublication str exp =
    let
        maybePub = List.head exp.publication
    in
   
    case maybePub of
        Nothing ->
            False

        Just publication ->
            publication.name == str   

isAuthorInExposition : String -> Exposition -> Bool
isAuthorInExposition str exp =
    let
        author = exp.author
    in
   
    author.name == str   

isExpositionUnpublished : Exposition -> Bool
isExpositionUnpublished exp =
    let
        maybePub = List.head exp.publication
    in
   
    case maybePub of
        Nothing ->
            True

        Just publication ->
            False  

filterExpositionsByKeywords : Expositions -> List String -> List Expositions
filterExpositionsByKeywords exps lst =
    List.map (filterExpositionsByKeyword exps) lst

filterExpositionsByKeyword : Expositions -> String -> Expositions
filterExpositionsByKeyword exps str =
    List.filter (isKwInKws str) exps

filterByPortal : String -> Expositions -> Expositions
filterByPortal str exps =
    if str == "all" then
        exps
    
    else if str == "not published" then
        let
            filtered = List.filter (isExpositionUnpublished) exps
        in
        filtered
    
    else
        let
            filtered = List.filter (isPortalInPublication str) exps
        in
        filtered

filterByJournal : String -> Expositions -> Expositions
filterByJournal str exps =
    if str == "all" then
        exps
    
    else if str == "not published" then
        let
            filtered = List.filter (isExpositionUnpublished) exps
        in
        filtered
    
    else
        let
            filtered = List.filter (isJournalInPublication str) exps
        in
        filtered

filterByAuthor : String -> Expositions -> Expositions
filterByAuthor str exps =
    if str == "all" then
        exps
    
    else 
        let
            filtered = List.filter (isAuthorInExposition str) exps
        in
        filtered


filterByKeyword : String -> Expositions -> Expositions
filterByKeyword str exps =
    List.filter (isKwInKws str) exps

tableHeaderFromField : String -> Html Msg
tableHeaderFromField field =
    th [ style "text-align" "left"] [ text field ]

tableDataFromField : Exposition -> String -> Html Msg
tableDataFromField exposition field =
    case field of
        "1|id" ->
            td [ style "border" "1px solid black" ] [ text (String.fromInt exposition.id) ]
        "2|title" ->     
            td [ style "border" "1px solid black" ] [ a [ href exposition.defaultPage ] [ text exposition.title ]]
        "3|author" ->
            td [ style "border" "1px solid black" ] [ text (getAuthor exposition) ]
        "4|keywords" ->
            kwToTableRow exposition.keywords   
        "5|publication" ->
            td [ style "border" "1px solid black" ] [ text (getPublication exposition) ]
        "6|abstract" ->
            td [ style "border" "1px solid black" ] [ text exposition.abstract ]
        _ ->
            td [ style "border" "1px solid black" ] [ text "no match" ]
    

thsFromFields : Model -> List (Html Msg)
thsFromFields model =
    let
        fields = model.fieldsToShow
        fieldsToList = Set.toList fields
    in

    List.map tableHeaderFromField fieldsToList

tdsFromFields : Model -> Exposition -> List (Html Msg)
tdsFromFields model exp =
    let
        fields = model.fieldsToShow
        fieldsToList = Set.toList fields
    in

    List.map (tableDataFromField exp) fieldsToList

viewTable : Model -> Expositions -> Html Msg
viewTable model exp =
    
    div []
        [ text ("Count|" ++ (String.fromInt (List.length exp)))
        , table [ style "borderta" "2px solid red", style "width" "200px" ]
        ( tr [] (thsFromFields model)
            :: List.map
                (\exposition ->
                    tr [] (tdsFromFields model exposition)
                )
                exp
        )       
    ]

rowItem : String -> Html msg
rowItem id =
    div []
        [ text id ]

buttonItem : String -> Html Msg
buttonItem id =
    button [onClick Noop]
        [ text id ]

keywordAsButton : String -> Html Msg
keywordAsButton kw =
    button [onClick (ClickedOnKeyword kw)]
        [ text kw ]


kwToTableRow : List String -> Html Msg
kwToTableRow kws =
    tr []
        [ td [ style "border" "1px solid black" ]
            (List.map keywordAsButton kws)
        ]

setPortal : String -> Model -> Model
setPortal str model=
    { model | portal = str}

setModelState : ModelState -> Model -> Model
setModelState state model=
    { model | state = state}

setModelFields : BrowseBy -> Model -> Model
setModelFields browse model=
    { model | browseBy = browse}

setModelPortal : String -> Model -> Model
setModelPortal portal model=
    { model | portal = portal}

setModelJournal : String -> Model -> Model
setModelJournal journal model=
    { model | journal = journal}

setModelAuthor : String -> Model -> Model
setModelAuthor author model=
    { model | author = author}

viewPortalAsButton : String -> Html Msg
viewPortalAsButton str =
    button [ onClick (SetPortal str)] [ text str ]   

viewJournalAsButton : String -> Html Msg
viewJournalAsButton str =
    button [ onClick (SetJournal str)] [ text str ] 

viewAuthorAsButton : String -> Html Msg
viewAuthorAsButton str =
    button [ onClick (SetAuthor str)] [ text str ] 

viewPortalsAsButtons : Set String -> List (Html Msg)
viewPortalsAsButtons set =
    let
        list = Set.toList set
    in
    
    List.map viewPortalAsButton list ++ 
    [button [ onClick (SetPortal "all")] [ text "-- all research --" ]]

viewJournalsAsButtons : Set String -> List (Html Msg)
viewJournalsAsButtons set =
    let
        list = Set.toList set
    in
    
    List.map viewJournalAsButton list ++ 
    [button [ onClick (SetJournal "all")] [ text "-- all research --" ]]

viewAuthorsAsButtons : Set String -> List (Html Msg)
viewAuthorsAsButtons set =
    let
        list = Set.toList set
    in
    
    List.map viewAuthorAsButton list

view : Model -> Html Msg
view model =
    div []
        [ div[]
            [ h2 [] [ text "RC Simple Browser" ] ]
        , div[]
            [ text "Load: "
            , button [ onClick (Load Local) ] [ text "Local" ]
            , button [ onClick (Load Web) ] [ text "Web" ]
            --, button [ onClick LoadPortals ] [ text "Load Portals" ]
            ]

        {-, div[]
            [ text "Show: "
            , button [ onClick (Show expositions Title) ] [ text "Title" ]
            , button [ onClick (Show expositions Abstract) ] [ text "Abstract" ]
            ] -}

        , div[]
            [ text "Browse: "
            , button [ onClick (Show expositions ByPortal) ] [ text "By Portal" ]
            , button [ onClick (Show expositions ByJournal) ] [ text "By Journal" ]
            , button [ onClick (Show expositions ByAuthor) ] [ text "By Author" ]
            , button [ onClick (Show expositions ByKeyword) ] [ text "By Keyword" ]
            ]

        , div[]
            [ text "Show: "
            , button [ onClick (SetField "1|id") ] [ text "Id" ]
            , button [ onClick (SetField "2|title") ] [ text "Title" ]
            , button [ onClick (SetField "3|author") ] [ text "Author" ]
            , button [ onClick (SetField "4|keywords") ] [ text "Keywords" ]
            , button [ onClick (SetField "5|publication") ] [ text "Publication" ]
            , button [ onClick (SetField "6|abstract") ] [ text "Abstract" ]
            ]

        , div[]
            --, h3 [] [text ("model state: " ++ (toString model))]
            [ --h1 [] [ text "table" ]
            --, h2 [] [ text (toString (List.length model.expositions))]
            --, h2 [] [ text (toString model.portals)]
            --, div[] (viewPortalsAsButtons model.portals)
            --, div[] (viewPortalsAsButtons model.journals)
            viewBrowserTitle model
            , viewBrowser model
            , viewModel model
            ]
        ]

viewBrowserTitle : Model -> Html Msg
viewBrowserTitle model =
    case model.browseBy of 
        ByPortal ->
            h3[] [ text ("Browsing portals")]--, showing: " ++ (toString model.fieldsToShow))]
        ByJournal ->
            h3[] [ text ("Browsing journals")]--, showing: " ++ (toString model.fieldsToShow))]
        ByAuthor ->
            h3[] [ text ("Browsing authors")]--, showing: " ++ (toString model.fieldsToShow))]
        ByKeyword ->
            h3[] [ text ("Browsing keywords")]--, showing: " ++ (toString model.fieldsToShow))]

viewBrowser : Model -> Html Msg
viewBrowser model =
    case model.browseBy of 
        ByPortal ->
            div[] (viewPortalsAsButtons model.portals)
        ByJournal ->
            div[] (viewJournalsAsButtons model.journals)
        ByAuthor ->
            div[] (viewAuthorBrowser model)
        ByKeyword ->
            div[] (viewKeywordBrowser model)


viewAuthorBrowser : Model -> List (Html Msg)
viewAuthorBrowser model =
    [ div[][input [ onInput SetAuthor ] []]
    , div[](viewAuthorsAsButtons (Set.filter (isStrInAuthor model.author) model.authors))
    ]

viewKeywordBrowser : Model -> List (Html Msg)
viewKeywordBrowser model =
    [ div[][input [ onInput SetKeyword ] []]
    --, div[](viewAuthorsAsButtons (Set.filter (isStrInAuthor model.author) model.authors))
    ]

isStrInAuthor : String -> String -> Bool
isStrInAuthor str author =
    String.contains str author

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

type alias Author =
    { id : Int
    , name : String
    }

type alias Publication =
    { id : Int
    , name : String
    }

type alias Portal =
    { name : String
    , count : Int
    }

publicationDecoder : Json.Decode.Decoder Publication
publicationDecoder =
    Json.Decode.map2 Publication
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)

authorDecoder : Json.Decode.Decoder Author
authorDecoder =
    Json.Decode.map2 Author
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)


expositionParser : Json.Decode.Decoder Exposition
expositionParser =
    Json.Decode.map7 Exposition
        (Json.Decode.field "abstract" Json.Decode.string)
        (Json.Decode.field "default-page" Json.Decode.string)
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "keywords" <| Json.Decode.list Json.Decode.string)
        (Json.Decode.field "published_in" <| Json.Decode.list publicationDecoder)
        (Json.Decode.field "author" authorDecoder)
        --(Json.Decode.field "meta-data-page" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)


expositionsParser : Json.Decode.Decoder Expositions
expositionsParser =
    Json.Decode.list expositionParser


loadExpositions : Location -> Cmd Msg
loadExpositions loc =
    let
        url =
            case loc of
                Web ->
                    "https://www.danielepozzi.com/rc.json"

                Local ->
                    "http://localhost:9000/rc.json"

        _ =
            Debug.log url

        --??
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotExpositions expositionsParser
        }


isFieldInFields : String -> Set String -> Bool
isFieldInFields field fields =
    Set.member field fields


updateFieldsToShow : Model -> String -> Model
updateFieldsToShow model field =
    let
        fields = model.fieldsToShow
        isFiF = isFieldInFields field fields
    in
    
    if isFiF then
        { model | fieldsToShow = Set.remove field model.fieldsToShow}
    else 
        { model | fieldsToShow = Set.insert field model.fieldsToShow}


{-
twoStatesFieldButton : Model -> String -> Html Msg
twoStatesFieldButton model field =
    let
        fields = model.fieldsToShow
        shown = Set.member field fields
    in
    case model.state of
        None ->
            button
                [ onClick (SetState (invertState model.state)) ]
                [ text (stateToString model.state) ]

        Show ->
            button
                [ onClick (SetState (invertState model.state)) ]
                [ text (stateToString model.state) ]
-}