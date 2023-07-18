module Main102attempt exposing (..)           -- Line 1     

import Browser                      -- Line 2
import Html exposing (div, text, input, button)    -- Line 3
import Html.Events exposing (onClick)
import String exposing (fromInt)
import Debug exposing (log)

-- Function to add two numbers      -- Line 4
add a b = a + b                     -- Line 5
init =                              -- Line 6
    { value = 33}                    -- Line 7

type Messages =
    Add

view model =                        -- Line 8
    div [] [
        text(fromInt model.value),
        div [] [],
        input [] [],
        button [onClick Add] [text "Add"]
    ]
update msg model =  
    let
        logmessage = log "here" "Button Clicked"
        logmessage2 = log "model" model
    in
    case msg of
        Add ->
            {value =  20}

main =                              -- Line 12
    Browser.sandbox                 -- Line 13
        {                           -- Line 14
            init = init,            -- Line 15
            view = view,            -- Line 16
            update = update         -- Line 17
        }    