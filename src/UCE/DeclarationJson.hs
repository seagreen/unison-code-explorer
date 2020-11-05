module UCE.DeclarationJson
  ( viewBodyJson,
    plainName,
    primaryName,
    refName,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.JSON.Generic
import UCE.Code
import UCE.Prelude
import Unison.HashQualified
import qualified Unison.Name as Name
import qualified Unison.Reference
import qualified Unison.Reference as R
import qualified Unison.Referent
import qualified Unison.ShortHash
import Unison.Util.AnnotatedText (AnnotatedText (..))
import qualified Unison.Util.Relation as Relation
import Unison.Util.SyntaxText (SyntaxText)
import qualified Unison.Util.SyntaxText as ST

data Ref = Ref
  { primaryName :: Text,
    alt :: Set Text,
    rhash :: Text
  }
  deriving (Show, Data, Typeable)

data Declaration = Declaration
  { name :: Ref,
    body :: [Segment],
    dependencies :: [Ref],
    dependents :: [Ref]
  }
  deriving (Show, Data, Typeable)

viewBodyJson :: CodeInfo -> Reference -> Declaration
viewBodyJson codeinfo ref =
  Declaration
    { name = refName ref codeinfo,
      body = case Map.lookup ref (codeBodies codeinfo) of
        Nothing ->
          []
        Just t -> toSegments t,
      dependencies = depList,
      dependents = mentionList
    }
  where
    depList :: [Ref]
    depList =
      let deps :: Set Reference
          deps =
            shallowDependencies ref (codeDependencies codeinfo)
       in namesToRefs deps

    mentionList :: [Ref]
    mentionList =
      let mentions :: Set Reference
          mentions =
            shallowReferences ref (codeDependencies codeinfo)
       in namesToRefs mentions

    namesToRefs :: Set Reference -> [Ref]
    namesToRefs =
      List.sortOn primaryName . fmap (\r -> (refName r codeinfo)) . Set.toList

refName :: Reference -> CodeInfo -> Ref
refName ref codeinfo =
  let (name, alt) = plainName codeinfo ref
   in Ref {primaryName = name, rhash = R.toText ref, alt = alt}

plainName :: CodeInfo -> Reference -> (Text, Set Text)
plainName codeinfo ref =
  case Set.toAscList <$> Map.lookup ref (Relation.domain (codeDeclarationNames codeinfo)) of
    Nothing ->
      (show ref, Set.empty)
    Just [] ->
      (show ref, Set.empty)
    Just [n] ->
      (Name.toText n, Set.empty)
    Just (x : others) ->
      (Name.toText x, Set.fromList (map Name.toText others))

-- * Segments

data Segment = Segment
  { contents :: String,
    kind :: SegmentKind
  }
  deriving (Show, Data, Typeable)

data SegmentKind
  = WithHash {segmentName :: String, hash :: String}
  | Other String
  | None
  deriving (Show, Data, Typeable)

toSegments :: SyntaxText -> [Segment]
toSegments (AnnotatedText items) =
  UCE.Prelude.map elementToSegments $ toList items

elementToSegments :: (String, Maybe ST.Element) -> Segment
elementToSegments (text, element) = case element of
  Nothing ->
    Segment {contents = text, kind = None}
  Just el ->
    Segment
      { contents = text,
        kind =
          case el of
            ST.Reference r ->
              WithHash
                { hash = Unison.ShortHash.toString . Unison.Reference.toShortHash $ r,
                  segmentName = "Reference"
                }
            ST.Referent r ->
              WithHash
                { hash = Unison.ShortHash.toString . Unison.Referent.toShortHash $ r,
                  segmentName = "Referent"
                }
            ST.HashQualifier hq ->
              case (Unison.HashQualified.toHash hq) of
                Nothing ->
                  Other "HashQualifier"
                Just hash ->
                  WithHash
                    { hash = Unison.ShortHash.toString hash,
                      segmentName = "HashQualifier"
                    }
            _ ->
              Other (show el)
      }
