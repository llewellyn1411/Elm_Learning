module Main exposing (..)

import Html exposing (Html, div, text, program)
import Mouse
import Keyboard

-- Model 

type alias Model = Int


init : (Model, Cmd Msg)
init = (0, Cmd.none)

-- Messages

type Msg = MouseMsg Mouse.Position | KeyMsg Keyboard.KeyCode

-- View

view : Model -> Html Msg
view model = div [][text (toString model)]

-- Update 

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MouseMsg position -> (model + 1, Cmd.none)

        KeyMsg code -> (model + 2, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [Mouse.clicks MouseMsg, Keyboard.downs KeyMsg]

-- Main

main: Program Never Model Msg
main = 
    program 
        {
            init= init,
            view= view,
            update= update,
            subscriptions = subscriptions
        }