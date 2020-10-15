module UCE.UI.Declaration where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.DOM.Events as P
import qualified Concur.Replica.DOM.Props as P
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import UCE.Code
import UCE.Prelude
import qualified Unison.Name as Name
import Unison.Util.AnnotatedText
import qualified Unison.Util.Relation as Relation
import Unison.Util.SyntaxText

viewBody :: CodeInfo -> Set Reference -> Widget HTML Reference
viewBody codeinfo refs =
  H.div
    [P.className "box"]
    [ H.pre
        []
        [ H.code
            []
            renderCode
        ],
      depTitle,
      H.ul [] depList,
      mentionTitle,
      H.ul [] mentionList
    ]
  where
    depTitle :: Widget HTML a
    depTitle =
      case depList of
        [] ->
          H.div [] []
        _ ->
          H.h5
            [P.className "title is-5 dependency-header"]
            [H.text "Dependencies"]

    mentionTitle :: Widget HTML a
    mentionTitle =
      case mentionList of
        [] ->
          H.div [] []
        _ ->
          H.h5
            [P.className "title is-5 dependency-header"]
            [H.text "Mentioned by"]

    depList :: [Widget HTML Reference]
    depList =
      let deps :: Set Reference
          deps =
            case Set.toList refs of
              [] ->
                mempty
              [r] ->
                shallowDependencies r (codeDependencies codeinfo)
              _ -> mempty -- todo
       in viewLink <$> namesToRefs deps

    mentionList :: [Widget HTML Reference]
    mentionList =
      let mentions :: Set Reference
          mentions =
            case Set.toAscList refs of
              [] ->
                mempty
              [r] ->
                shallowReferences r (codeDependencies codeinfo)
              _ -> mempty -- todo
       in viewLink <$> namesToRefs mentions

    viewLink :: (Text, Reference) -> Widget HTML Reference
    viewLink (name, ref) = do
      _ <-
        H.li
          [P.onClick]
          [ H.a
              []
              [H.text name]
          ]
      pure ref

    namesToRefs :: Set Reference -> [(Text, Reference)]
    namesToRefs =
      List.sortOn fst . fmap (\r -> (refName r codeinfo, r)) . Set.toList

    renderCode :: [Widget HTML Reference]
    renderCode =
      case Set.toAscList refs of
        [] ->
          [H.text "<programmer error: no ref>"]
        [r] ->
          case Map.lookup r (codeBodies codeinfo) of
            Nothing ->
              [H.text "<not found>"]
            Just st ->
              syntaxTextToWidgets st
        _ ->
          [H.text "<conflicted name>"]

    syntaxTextToWidgets :: SyntaxText -> [Widget HTML Reference]
    syntaxTextToWidgets (AnnotatedText elements) =
      viewElement <$> toList elements
      where
        viewElement :: (String, Maybe Element) -> Widget HTML Reference
        viewElement (name, mElement) =
          let def = H.text (toText name)
           in case mElement of
                Nothing ->
                  def
                Just el ->
                  case el of
                    Reference r -> do
                      _ <- H.a [P.onClick] [def]
                      pure r
                    _ ->
                      def

refName :: Reference -> CodeInfo -> Text
refName ref codeinfo =
  case Set.toAscList <$> Map.lookup ref (Relation.domain (codeDeclarationNames codeinfo)) of
    Nothing ->
      show ref
    Just [] ->
      show ref
    Just [n] ->
      Name.toText n
    Just (x : y : []) ->
      Name.toText x <> " (also called " <> Name.toText y <> ")"
    Just (x : y : _) ->
      Name.toText x <> " (also called " <> Name.toText y <> " and others)"
