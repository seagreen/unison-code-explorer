module UCE.App where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import UCE.CodeInfo
import UCE.Prelude

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.Events as P
import qualified Concur.Replica.Props as P
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.Name as Name

app :: CodeInfo -> Widget HTML a
app codeinfo = do
  liftIO (logLn "Running app")
  H.div []
    [ welcome
    , search codeinfo mempty mempty
    ]
  where
    welcome :: Widget HTML a
    welcome =
      H.div [P.className "box"]
        [ H.p []
            [ H.text "Welcome to Unison Code Explorer. This is a work in progress ("
            , H.a [P.href "https://github.com/seagreen/unison-code-explorer"]
                [H.text "GitHub link"]
            , H.text ")."
            ]
        ]

newtype OpenNames
  = OpenNames { unOpenNames :: Set Name }
  deriving newtype (Semigroup, Monoid)

search :: CodeInfo -> Text -> OpenNames -> Widget HTML a
search codeinfo searchStr openNames = do
  res <-
    H.div []
      [ One2 <$> searchBox
      , Two2 <$> results
      ]
  case res of
    One2 t ->
      search codeinfo t openNames

    Two2 newOpenNames ->
      search codeinfo searchStr newOpenNames
  where
    searchBox :: Widget HTML Text
    searchBox = do
      e <-
        H.div []
          [ H.input
            [ P.className "input"
            , P.autofocus True
            , P.placeholder "Search string"
            , P.value searchStr
            , P.onInput
            , P.type_ "text"
            ]
          ]
      pure (P.targetValue (P.target e))

    results :: Widget HTML OpenNames
    results =
      H.ul []
        (codeinfo
          & apiNames
          & Map.filterWithKey (\n _ -> Text.isInfixOf strLower (Text.toLower (Name.toText n)))
          & Map.keys
          & map viewName)
      where
        strLower :: Text
        strLower =
          Text.toLower searchStr

    viewName :: Name -> Widget HTML OpenNames
    viewName name = do
      _ <-
        H.li []
          [ H.button [P.onClick, P.className "button"]
              [ H.text (btn <> " " <> Name.toText name)
              ]
          ]
      pure (OpenNames (setSwap name (unOpenNames openNames)))
      where
        btn :: Text
        btn
          | Set.member name (unOpenNames openNames) = "-"
          | otherwise                               = "+"
