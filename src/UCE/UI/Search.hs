module UCE.UI.Search where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.DOM.Events as P
import qualified Concur.Replica.DOM.Props as P
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import UCE.Code
import UCE.Prelude
import UCE.UI.Declaration
import Unison.Name (Name)
import qualified Unison.Name as Name
import qualified Unison.Util.Relation as Relation

newtype OpenNames = OpenNames {unOpenNames :: Set Name}
  deriving newtype (Semigroup, Monoid)

search :: CodeInfo -> Text -> OpenNames -> Widget HTML Reference
search codeinfo searchStr openNames = do
  res <-
    H.div
      []
      [ One2 <$> searchBox,
        Two2 <$> results
      ]
  case res of
    One2 t ->
      search codeinfo t openNames
    Two2 (Left newOpenNames) ->
      search codeinfo searchStr newOpenNames
    Two2 (Right ref) ->
      pure ref
  where
    searchBox :: Widget HTML Text
    searchBox = do
      e <-
        H.div
          []
          [ H.input
              [ P.className "input search-box",
                P.autofocus True,
                P.placeholder "Search string",
                P.value searchStr,
                P.onInput,
                P.type_ "text"
              ]
          ]
      pure (P.targetValue (P.target e))

    results :: Widget HTML (Either OpenNames Reference)
    results =
      H.ul
        []
        ( codeinfo
            & codeDeclarationNames
            & Relation.range
            & Map.filterWithKey (\n _ -> Text.isInfixOf strLower (Text.toLower (Name.toText n)))
            & Map.toAscList
            & map viewResult
        )
      where
        strLower :: Text
        strLower =
          Text.toLower searchStr

    viewResult :: (Name, Set Reference) -> Widget HTML (Either OpenNames Reference)
    viewResult (name, refs) = do
      H.li
        [P.className "search-result"]
        [ Left (OpenNames (setSwap name (unOpenNames openNames)))
            <$ H.button
              [P.onClick, P.className "button"]
              [ H.text (btn <> " " <> Name.toText name)
              ],
          Right <$> body
        ]
      where
        isOpen :: Bool
        isOpen =
          Set.member name (unOpenNames openNames)

        btn :: Text
        btn
          | isOpen = "-"
          | otherwise = "+"

        body :: Widget HTML Reference
        body
          | not isOpen = H.div [] []
          | otherwise = viewBody codeinfo refs
