module UCE.UI where

import Concur.Core (Widget)
import Concur.Replica (HTML)
import UCE.Code
import UCE.Prelude

import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.Events as P
import qualified Concur.Replica.Props as P
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.Name as Name

data State = Searching | ViewSingle Reference

app :: CodeInfo -> State -> Widget HTML a
app codeinfo state = do
  liftIO (logLn "Running app")
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

newtype OpenNames
  = OpenNames { unOpenNames :: Set Name }
  deriving newtype (Semigroup, Monoid)

search :: CodeInfo -> Text -> OpenNames -> Widget HTML Reference
search codeinfo searchStr openNames = do
  res <-
    H.div []
      [ One2 <$> searchBox
      , Two2 <$> results
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

    results :: Widget HTML (Either OpenNames Reference)
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

    viewTerm :: (Name, Set Reference) -> Widget HTML (Either OpenNames Reference)
    viewTerm (name, refs) = do
      H.li []
        [ Left (OpenNames (setSwap name (unOpenNames openNames)))
            <$ H.button [P.onClick, P.className "button"]
                 [ H.text (btn <> " " <> Name.toText name)
                 ]
        , Right <$> body
        ]
      where
        isOpen :: Bool
        isOpen =
          Set.member name (unOpenNames openNames)

        btn :: Text
        btn
          | isOpen    = "-"
          | otherwise = "+"

        body :: Widget HTML Reference
        body
          | not isOpen = H.div [] []
          | otherwise  = viewBody codeinfo refs

viewBody :: CodeInfo -> Set Reference -> Widget HTML Reference
viewBody codeinfo refs =
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
          case Map.lookup r (termBodies codeinfo) of
            Nothing ->
              "<not found>"

            Just t ->
              t

        _ ->
          "<confliced name>"

refName :: Reference -> CodeInfo -> Text
refName ref codeinfo =
  case Set.toList <$> Map.lookup ref (apiRefsToNames codeinfo) of
    Nothing ->
      showText ref

    Just [] ->
      showText ref

    Just [n] ->
      Name.toText n

    Just (x:y:[]) ->
      Name.toText x <> " (also " <> Name.toText y <> ")"

    Just (x:y:_) ->
      Name.toText x <> " (also " <> Name.toText y <> " and others)"
