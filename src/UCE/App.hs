module UCE.App where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import UCE.Load
import UCE.Prelude

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.Props as P
import qualified Data.Map as Map

app :: API -> Widget HTML a
app codebase = do
  liftIO (logLn "Running app")
  H.div []
    [ H.p []
        [ H.text "Welcome to Unison Code Explorer. This is a work in progress ("
        , H.a [P.href "https://github.com/seagreen/unison-code-explorer"]
            [H.text "GitHub link"]
        , H.text ")."
        ]
    , H.ul []
        (fmap viewName (Map.elems (unNames (apiNames codebase))))
    ]

viewName :: Text -> Widget HTML a
viewName name =
  H.li []
    [ H.text name
    ]
