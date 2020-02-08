module UCE.UI.Declaration where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import UCE.Code
import UCE.Prelude

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.Events as P
import qualified Concur.Replica.Props as P
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Unison.Name as Name

viewBody :: CodeInfo -> Set Reference -> Widget HTML Reference
viewBody codeinfo refs =
  H.div [P.className "box"]
    [ H.pre []
        [ H.code []
            [H.text txt]
        ]
    , H.ul []
        (viewLink <$> callList)
    ]
  where
    callList :: [Reference]
    callList =
      case Set.toList refs of
        [] ->
          mempty

        [r] ->
          Set.toList (functionCalls r (apiFcg codeinfo))

        _ -> mempty -- todo

    viewLink :: Reference -> Widget HTML Reference
    viewLink ref = do
      _ <-
        H.li [P.onClick]
          [ H.a []
              [H.text (refName ref codeinfo)]
          ]
      pure ref
      where

    txt =
      case Set.toList refs of
        [] ->
          "<programmer error: no ref>"

        [r] ->
          case Map.lookup r (termBodies codeinfo) of
            Nothing ->
              "<not found>"

            Just t ->
              t

        _ ->
          "<confliced name>"

refName :: Reference -> CodeInfo -> Text
refName ref codeinfo =
  case Set.toList <$> Map.lookup ref (apiRefsToNames codeinfo) of
    Nothing ->
      showText ref

    Just [] ->
      showText ref

    Just [n] ->
      Name.toText n

    Just (x:y:[]) ->
      Name.toText x <> " (also " <> Name.toText y <> ")"

    Just (x:y:_) ->
      Name.toText x <> " (also " <> Name.toText y <> " and others)"
