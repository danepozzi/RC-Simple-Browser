module Main01textfields exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { tb1 : String
  , tb2: String
  }


init : Model
init =
  { tb1 = ""
  , tb2 = "eccomi"
  }



-- UPDATE


type Msg
  = Change String
  | Repeat String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | tb1 = newContent }
    Repeat newContent ->
      { model | tb2 = newContent }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Text to reverse", value model.tb1, onInput Change ][]
    , div [] [ text (String.reverse model.tb1) ]
    , div [] [ text ("charcount: " ++ (String.fromInt (String.length model.tb1))) ]
    , input [ placeholder "Text to repeat", value model.tb2, onInput Repeat ][]
    , div [] [ text (String.repeat 30 model.tb2) ]
    , div [] [ text ("charcount: " ++ (String.fromInt (String.length (String.repeat 7 model.tb2)))) ]
    ]
