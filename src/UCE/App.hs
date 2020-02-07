module UCE.App where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import UCE.CodeInfo
import UCE.Prelude

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.Events as P
import qualified Concur.Replica.Props as P
import qualified Data.List as List
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
          & Map.toList
          & List.sortOn fst
          & map viewTerm)
      where
        strLower :: Text
        strLower =
          Text.toLower searchStr

    viewTerm :: (Name, Set Reference) -> Widget HTML OpenNames
    viewTerm (name, refs) = do
      _ <-
        H.li []
          [ H.button [P.onClick, P.className "button"]
              [ H.text (btn <> " " <> Name.toText name)
              ]
          , body
          ]
      pure (OpenNames (setSwap name (unOpenNames openNames)))
      where
        isOpen :: Bool
        isOpen =
          Set.member name (unOpenNames openNames)

        btn :: Text
        btn
          | isOpen    = "-"
          | otherwise = "+"

        body :: Widget HTML a
        body
          | not isOpen = H.div [] []
          | otherwise  =
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

            viewLink :: Reference -> Widget HTML a
            viewLink ref =
              H.div []
                [H.text refName]

              where
                refName :: Text
                refName =
                  case Set.toList <$> Map.lookup ref (apiRefsToNames codeinfo) of
                    Nothing ->
                      showText ref

                    Just [] ->
                      showText ref

                    Just [n] ->
                      Name.toText n

                    Just (x:y:_) -> -- NOTE: only shows two
                      "Name conflicted, first is: " <> Name.toText x <> " second is: " <> Name.toText y

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
