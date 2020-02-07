module UCE.App where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import UCE.Load
import UCE.Prelude

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.Events as P
import qualified Concur.Replica.Props as P
import qualified Data.Map as Map
import qualified Data.Text as Text

app :: CodeInfo -> Widget HTML a
app codeinfo = do
  liftIO (logLn "Running app")
  H.div []
    [ welcome
    , search codeinfo mempty
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

search :: CodeInfo -> Text -> Widget HTML a
search codeinfo str = do
  t <-
    H.div []
      [ searchBox
      , results
      ]
  search codeinfo t
  where
    searchBox :: Widget HTML Text
    searchBox = do
      e <-
        H.div []
          [ H.input
            [ P.className "input"
            , P.autofocus True
            , P.placeholder "Search string"
            , P.value str
            , P.onInput
            , P.type_ "text"
            ]
          ]
      pure (P.targetValue (P.target e))

    results :: Widget HTML a
    results =
      H.ul []
        (codeinfo
          & apiNames
          & unNames
          & Map.filter (\n -> Text.isInfixOf strLower (Text.toLower n))
          & Map.elems
          & map viewName)
      where
        strLower :: Text
        strLower =
          Text.toLower str

    viewName :: Text -> Widget HTML a
    viewName name =
      H.li []
        [ H.button [P.className "button"]
            [ H.text ("+ " <> name)
            ]
        ]
