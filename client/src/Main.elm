module Main exposing (main)

import Browser
import Dict.Any as Dict exposing (AnyDict)
import Element as El exposing (Element)
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
    { data : Remote String Data
    }


type alias Data =
    { functions : AnyDict String (Id Function) Function
    , calls : List Call
    }


type Remote e a
    = InitialLoad
    | InitialFail e
    | Success a
    | LoadingAgain a
    | Error e a


type Id a
    = Id String


type alias Function =
    { name : String }


type alias Call =
    { caller : Id Function
    , callee : Id Function
    }


type Msg
    = Everything (Result String Data)


init : ( Model, Cmd Msg )
init =
    ( { data = InitialLoad }, getEverything )


getEverything : Cmd Msg
getEverything =
    Task.attempt Everything
        (Task.succeed
            { functions =
                Dict.fromList (\(Id s) -> s)
                    [ ( Id "a", { name = "foo" } )
                    , ( Id "b", { name = "bar" } )
                    ]
            , calls =
                [ { caller = Id "a"
                  , callee = Id "b"
                  }
                ]
            }
        )


{-| using Maybe to model "did this msg update the state?"
-}
update : Msg -> Model -> Maybe Model
update msg model =
    case msg of
        Everything res ->
            Maybe.map (\x -> { data = x }) <|
                case model.data of
                    InitialLoad ->
                        Just <|
                            case res of
                                Ok data ->
                                    Success data

                                Err e ->
                                    InitialFail e

                    LoadingAgain a ->
                        Just <|
                            case res of
                                Ok data ->
                                    Success data

                                Err e ->
                                    Error e a

                    InitialFail _ ->
                        Nothing

                    Success _ ->
                        Nothing

                    Error _ _ ->
                        Nothing


view : Model -> Element Msg
view model =
    case model.data of
        InitialLoad ->
            El.text "Loading"

        InitialFail err ->
            El.text err

        Success a ->
            El.text "success"

        LoadingAgain a ->
            El.text "success"

        Error e a ->
            El.text e
