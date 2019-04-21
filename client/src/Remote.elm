module Remote exposing (Remote(..), update)


type Remote e a
    = InitialLoad
    | InitialFail e
    | Success a
    | LoadingAgain a
    | Error e a


update : Result e a -> Remote e a -> Maybe (Remote e a)
update res remote =
    case remote of
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
