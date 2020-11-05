module UCE.Static.Organize where

import qualified Data.Map.Strict as Map
import qualified Data.Text
import UCE.Code
import UCE.DeclarationJson (plainName)
import UCE.Prelude
import qualified Unison.Hash as Hash
import qualified Unison.Reference as Reference
import Unison.ShortHash (ShortHash (..))
import qualified Unison.Util.Relation as Relation
import qualified Prelude

type ScopeMap = Map [Text] (Maybe Reference, [([Text], Reference)])

splitParts :: Text -> [Text]
splitParts name = fixDouble $ Data.Text.splitOn "." name

fixDouble :: [Text] -> [Text]
fixDouble ("" : "" : rest) = "." : fixDouble rest
fixDouble (head' : rest) = head' : fixDouble rest
fixDouble [] = []

dots :: [Text] -> Text
dots [] = "Home"
dots x = Data.Text.intercalate "." x

parentPath :: [a] -> [a]
parentPath [] = []
parentPath path = reverse path & Prelude.tail & reverse

indexHref :: [Text] -> Text
indexHref [] = "index"
indexHref path = dots path

itemHref :: Reference -> Text
itemHref (Reference.Builtin b) = "__builtin__" <> b
itemHref (Reference.DerivedId (Reference.Id h _ 1)) = (Hash.base32Hex h)
itemHref (Reference.DerivedId (Reference.Id h i n)) = (Hash.base32Hex h) <> Reference.showSuffix i n

makeHref :: [Text] -> Maybe Reference -> ChildMap -> EntryMap -> Text
makeHref path Nothing _ _ = indexHref path <> ".html"
makeHref path (Just child) children scopeMap =
  if Map.size children == 0
    then
      ( case (Map.lookup (parentPath path) scopeMap) of
          Nothing -> "FAILURE" & Data.Text.pack
          Just (Nothing, _) -> indexHref (parentPath path)
          Just (Just r, _) -> itemHref r
      )
        <> ".html#"
        <> itemHref child
    else itemHref child <> ".html"

makeHrefMap :: EntryMap -> Map [Text] Text
makeHrefMap scopeMap =
  Map.mapWithKey f scopeMap
  where
    f path (ref, children) = makeHref path ref children scopeMap

type ChildMap = Map [Text] (Maybe Reference)

type EntryMap = Map [Text] (Maybe Reference, ChildMap)

makeScopeMap :: CodeInfo -> ([([Text], Reference)], Map [Text] (Maybe Reference, ChildMap))
makeScopeMap codeinfo =
  let names = codeinfo & UCE.Code.codeDeclarationNames & Relation.domain & Map.toAscList
      refs = map fst names
      paths = map (\r -> (plainName codeinfo r & fst & splitParts, r)) refs
   in (paths, foldl' f Map.empty paths)
  where
    addToChildren :: ChildMap -> [Text] -> (Maybe Reference) -> ChildMap
    addToChildren children path Nothing = case (Map.lookup path children) of
      Nothing -> Map.insert path Nothing children
      _ -> children
    addToChildren children path (Just ref) = Map.insert path (Just ref) children
    addEntry entryMap path ref childMap =
      let entryMap' = case (Map.lookup path entryMap) of
            Nothing -> Map.insert path (ref, childMap) entryMap
            Just (_, children) -> Map.insert path (ref, children) entryMap
       in addToParent entryMap' path ref
    addToParent mmap [] _ = mmap
    addToParent mmap path r =
      case (Map.lookup parent mmap) of
        Nothing -> addEntry mmap parent Nothing (Map.fromList [(path, r)])
        Just (item, children) -> Map.insert parent (item, addToChildren children path r) mmap
      where
        parent = parentPath path
    f mmap (path, ref) = addEntry mmap path (Just ref) Map.empty

makeHashMap :: Foldable t => t (a, Reference) -> Map ShortHash a
makeHashMap paths = foldl' f Map.empty paths
  where
    f mmap (path, r) = Map.insert (Reference.toShortHash r) path mmap

isChild :: Eq a => [a] -> [a] -> Bool
isChild base ref = base == parentPath ref

childRefs :: Eq a => [a] -> [([a], b)] -> [([a], b)]
childRefs base refs =
  filter (isChild base . fst) refs
