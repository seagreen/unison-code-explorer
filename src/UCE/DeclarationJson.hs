module UCE.DeclarationJson where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.JSON.Generic
import UCE.Code
import UCE.Code.Print (Segment (..), toSegments)
import UCE.Prelude
import qualified Unison.Name as Name
import qualified Unison.Reference as R
import qualified Unison.Util.Relation as Relation

data Ref = Ref {primaryName :: Text, alt :: Set Text, rhash :: Text} deriving (Show, Data, Typeable)

data Declaration = Declaration
  { name :: Ref,
    body :: [Segment],
    dependencies :: [Ref],
    dependents :: [Ref]
  }
  deriving (Show, Data, Typeable)

viewBody :: CodeInfo -> Reference -> Declaration
viewBody codeinfo ref =
  Declaration
    { name = (refName ref codeinfo),
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
plainName codeinfo ref = case Set.toAscList <$> Map.lookup ref (Relation.domain (codeDeclarationNames codeinfo)) of
  Nothing ->
    (showText ref, Set.empty)
  Just [] ->
    (showText ref, Set.empty)
  Just [n] ->
    (Name.toText n, Set.empty)
  Just (x : others) ->
    (Name.toText x, Set.fromList (map Name.toText others))
