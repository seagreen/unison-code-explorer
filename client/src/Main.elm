module Main exposing (main)

import Browser
import Element as El
import Html exposing (..)


type alias Model =
    Int


type alias Msg =
    Bool


main : Program () Model Msg
main =
    Browser.sandbox
        { init = 0
        , update = \_ model -> model
        , view = El.layout [] << view
        }


view : Model -> El.Element Msg
view _ =
    El.none
