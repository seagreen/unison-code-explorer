module UCE.UI where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import UCE.Code
import UCE.Prelude
import UCE.UI.Declaration
import UCE.UI.Search

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.Events as P
import qualified Concur.Replica.Props as P
import qualified Data.Set as Set

data State = Searching | ViewSingle Reference

app :: CodeInfo -> State -> Widget HTML a
app codeinfo state = do
  newState <-
    H.div []
      [ welcome
      , case state of
          Searching ->
            ViewSingle <$> search codeinfo mempty mempty

          ViewSingle ref ->
            viewSingle codeinfo ref
      ]
  app codeinfo newState
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

viewSingle :: CodeInfo -> Reference -> Widget HTML State
viewSingle codeinfo ref =
  H.div []
    [ H.h3 [P.className "title is-3"]
        [H.text (refName ref codeinfo)]
    , ViewSingle <$> viewBody codeinfo (Set.singleton ref)
    , Searching <$ backToSearch
    ]
  where
    backToSearch :: Widget HTML P.MouseEvent
    backToSearch =
      H.button [P.onClick, P.className "button"]
        [ H.text "Back to search"
        ]
