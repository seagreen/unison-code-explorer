module Main exposing (main)

import Browser
import Dict.Any as Dict exposing (AnyDict)
import Element as El exposing (Element)
import Http
import Json.Decode as JD exposing (Decoder)
import Remote exposing (Remote(..))
import Task exposing (Task)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update =
            \msg model ->
                update msg model
                    |> Maybe.withDefault model
                    |> (\x -> ( x, Cmd.none ))
        , view = El.layout [] << view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { functions : Remote Http.Error Functions
    , calls : Remote Http.Error Calls
    }


type alias Calls =
    List Call


type alias Functions =
    AnyDict String (Id Function) Function


type Id a
    = Id String


type alias Function =
    { name : String }


type alias Call =
    { caller : Id Function
    , callee : Id Function
    }


type Msg
    = GotCalls (Result Http.Error Calls)
    | GotFunctions (Result Http.Error Functions)


init : ( Model, Cmd Msg )
init =
    ( { calls = InitialLoad
      , functions = InitialLoad
      }
    , getEverything
    )


getEverything : Cmd Msg
getEverything =
    Cmd.batch
        [ Http.get
            { url = "/function-call-graph"
            , expect =
                Http.expectJson GotCalls
                    decodeCalls
            }
        , Http.get
            { url = "/names"
            , expect =
                Http.expectJson GotFunctions
                    decodeFunctions
            }
        ]


decodeCalls : Decoder Calls
decodeCalls =
    JD.list
        (JD.map2 Tuple.pair
            (JD.index 0 funcId)
            (JD.index 1 (JD.list funcId))
        )
        |> JD.map
            (\raw ->
                List.concatMap
                    (\( caller, callees ) ->
                        List.map (Call caller) callees
                    )
                    raw
            )


decodeFunctions : Decoder Functions
decodeFunctions =
    -- TODO actually parse
    JD.succeed (Dict.fromList (\(Id s) -> s) [])


funcId : Decoder (Id Function)
funcId =
    JD.map Id JD.string


{-| using Maybe to model "did this msg update the state?"
-}
update : Msg -> Model -> Maybe Model
update msg model =
    case msg of
        GotFunctions res ->
            Maybe.map (\x -> { model | functions = x }) <|
                Remote.update res model.functions

        GotCalls res ->
            Maybe.map (\x -> { model | calls = x }) <|
                Remote.update res model.calls


view : Model -> Element Msg
view model =
    case model.calls of
        InitialLoad ->
            El.text "Loading"

        InitialFail err ->
            El.text (Debug.toString err)

        Success a ->
            El.text "success"

        LoadingAgain a ->
            El.text "success"

        Error e a ->
            El.text (Debug.toString e)
