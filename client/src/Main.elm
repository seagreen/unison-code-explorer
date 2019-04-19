module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element as El
import Html exposing (..)


type alias Model =
    { data : Remote String Data
    }


type alias Data =
    { functions : Dict String Function
    , calls : List Call
    }


type Remote e a
    = InitialLoad
    | InitialFail e
    | Success a
    | LoadingAgain a
    | Error e a


type alias Function =
    { hash : String
    , name : String
    }


type alias Call =
    { caller : String
    , callee : String
    }


type alias Msg =
    Bool


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { data = InitialLoad }
        , update = \_ model -> model
        , view = El.layout [] << view
        }


view : Model -> El.Element Msg
view _ =
    El.none
