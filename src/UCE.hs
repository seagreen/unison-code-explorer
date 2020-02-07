module UCE where

import Concur.Replica (Attr(..), Attrs, HTML, VDOM(..), clientDriver)
import Data.Text.Encoding (decodeUtf8)
import Network.WebSockets (defaultConnectionOptions)
import UCE.Code
import UCE.Prelude
import UCE.UI (State(Searching), app)

import qualified Concur.Replica.Run
import qualified Data.Map as Map
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Static as Static

run :: Int -> CodeInfo -> IO ()
run port codeinfo =
  Concur.Replica.Run.run
    port
    index
    defaultConnectionOptions
    static
    (app codeinfo Searching)

static :: Wai.Middleware
static =
  Static.staticPolicy $
    Static.only [("custom.css", "custom.css"), ("bulmaswatch.min.css", "bulmaswatch.min.css")]

index :: HTML
index =
  [ VLeaf "meta" (fl [("charset", AText "utf-8")]) Nothing
  , VLeaf "!doctype" (fl [("html", ABool True)]) Nothing
  , VNode "html" mempty Nothing
      [ VNode "head" mempty Nothing
          [ VNode "title" mempty Nothing [VText "Unison Code Explorer"]
          , VNode "link"
              (fl [ ("href", AText "./bulmaswatch.min.css")
                  , ("rel", AText "stylesheet")
                  ])
              Nothing
              []
          , VNode "link"
              (fl [ ("href", AText "custom.css")
                  , ("rel", AText "stylesheet")
                  ])
              Nothing
              []
          ]
      , VNode "body" mempty Nothing
          [ VNode "script" (fl [("language", AText "javascript")]) Nothing
              [ VRawText $ decodeUtf8 clientDriver ]
          ]
      ]
  ]
  where
    fl :: [(Text, Attr)] -> Attrs
    fl = Map.fromList
