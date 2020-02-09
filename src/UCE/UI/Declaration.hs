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
import qualified Unison.Util.Relation as Relation

viewBody :: CodeInfo -> Set Reference -> Widget HTML Reference
viewBody codeinfo refs =
  H.div [P.className "box"]
    [ H.pre []
        [ H.code []
            [H.text txt]
        ]
    , depTitle
    , H.ul []
        (viewLink <$> depList)
    , mentionTitle
    , H.ul []
        (viewLink <$> mentionList)
    ]
  where
    depTitle :: Widget HTML a
    depTitle =
      case depList of
        [] ->
          H.div [] []

        _ ->
          H.h5 [P.className "title is-5"]
            [H.text "Dependencies" ]

    mentionTitle :: Widget HTML a
    mentionTitle =
      case mentionList of
        [] ->
          H.div [] []

        _ ->
          H.h5 [P.className "title is-5"]
            [H.text "Mentioned by" ]

    depList :: [Reference]
    depList =
      case Set.toList refs of
        [] ->
          mempty

        [r] ->
          Set.toList (shallowDependencies r (codeDependencies codeinfo))

        _ -> mempty -- todo

    mentionList :: [Reference]
    mentionList =
      case Set.toList refs of
        [] ->
          mempty

        [r] ->
          Set.toList (shallowReferences r (codeDependencies codeinfo))

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
          case Map.lookup r (codeBodies codeinfo) of
            Nothing ->
              "<not found>"

            Just t ->
              t

        _ ->
          "<confliced name>"

refName :: Reference -> CodeInfo -> Text
refName ref codeinfo =
  case Set.toList <$> Map.lookup ref (Relation.domain (codeDeclarationNames codeinfo)) of
    Nothing ->
      showText ref

    Just [] ->
      showText ref

    Just [n] ->
      Name.toText n

    Just (x:y:[]) ->
      Name.toText x <> " (also called " <> Name.toText y <> ")"

    Just (x:y:_) ->
      Name.toText x <> " (also called " <> Name.toText y <> " and others)"
