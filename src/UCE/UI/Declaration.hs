module UCE.UI.Declaration where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import UCE.Code
import UCE.Prelude

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.Events as P
import qualified Concur.Replica.Props as P
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Unison.Name as Name
import qualified Unison.Util.Relation as Relation

viewBody :: CodeInfo -> Set Reference -> Widget HTML Reference
viewBody codeinfo refs =
  H.div [P.className "box"]
    [ H.pre []
        [ H.code []
            [H.text bodyTxt]
        ]
    , depTitle
    , H.ul [] depList
    , mentionTitle
    , H.ul [] mentionList
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

    depList :: [Widget HTML Reference]
    depList =
      let
        deps :: Set Reference
        deps =
          case Set.toList refs of
            [] ->
              mempty

            [r] ->
              shallowDependencies r (codeDependencies codeinfo)

            _ -> mempty -- todo
      in
        viewLink <$> namesToRefs deps

    mentionList :: [Widget HTML Reference]
    mentionList =
      let
        mentions :: Set Reference
        mentions =
          case Set.toAscList refs of
            [] ->
              mempty

            [r] ->
              shallowReferences r (codeDependencies codeinfo)

            _ -> mempty -- todo
      in
        viewLink <$> namesToRefs mentions

    viewLink :: (Text, Reference) -> Widget HTML Reference
    viewLink (name, ref) = do
      _ <-
        H.li [P.onClick]
          [ H.a []
              [H.text name]
          ]
      pure ref

    namesToRefs :: Set Reference -> [(Text, Reference)]
    namesToRefs =
      List.sortOn fst . fmap (\r -> (refName r codeinfo, r)) . Set.toList

    bodyTxt :: Text
    bodyTxt =
      case Set.toAscList refs of
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
  case Set.toAscList <$> Map.lookup ref (Relation.domain (codeDeclarationNames codeinfo)) of
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
