-- https://ellie-app.com/bJSMQz9tydqa1
module Main03celsius exposing (..)

import Browser
import Html exposing (Html, Attribute, span, input, text, div)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { celsius : String
  , meters : Int
  }


init : Model
init =
  { celsius = "" 
  , meters = 0
  }



-- UPDATE


type Msg
  = Change String
  | Inches String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newInput ->
      { model | celsius = newInput }
    
    Inches newInput ->
      {  model | meters = String.toInt newInput }



-- VIEW


view : Model -> Html Msg
view model =
  case String.toFloat model.celsius of
    Just celsius ->
      viewConverter model.celsius "blue" "black" (String.fromFloat (celsius * 1.8 + 32))

    Nothing ->
      viewConverter model.celsius "red" "red" "???"

  String.toFloat model.meters of
    Just celsius ->
      viewConverter model.celsius "blue" "black" (String.fromFloat (celsius * 1.8 + 32))

    Nothing ->
      viewConverter model.celsius "red" "red" "???"


viewConverter : String -> String -> String -> String -> Html Msg
viewConverter userInput color bcolor equivalentTemp =
  div[]
  [
  span []
    [ input [ value userInput, onInput Change, style "width" "40px", style "border-color" bcolor] []
    , text "°C = "
    , span [ style "color" color ] [ text equivalentTemp ]
    , text "°F"
    , div[]
  [
  span []
    [ input [ value userInput, onInput Inches, style "width" "40px", style "border-color" bcolor] []
    , text "°m = "
    , span [ style "color" color ] [ text equivalentTemp ]
    , text "°i"
    ]
  ]
    ]
  ]
