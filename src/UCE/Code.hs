-- | Use the Unison compiler as a library to get info about a codebase.
module UCE.Code
  ( load,
    CodeInfo (..),
    DependencyGraph (..),
    shallowDependencies,
    shallowReferences,
  )
where

import Data.Map.Strict ()
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO
import System.IO (stderr)
import UCE.Code.LoadCodebase (loadCodebaseAndBranch)
import UCE.Code.Print
import UCE.Prelude hiding (head)
import UCE.Static.DisplayDoc as DisplayDoc
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Runtime as Runtime
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as Decl
import Unison.Name (Name)
import Unison.Parser (Ann (..))
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Runtime.Rt1IO as Rt1
import qualified Unison.Term as Term
import qualified Unison.Util.Relation as Relation

-- A Referent can be a value, function, or constructor.
--
-- data Referent = Ref Reference | Con Reference Int ConstructorType

-- data Reference
--   = Builtin Text.Text
--   | DerivedId Id deriving (Eq,Ord,Generic)

-- Pos and Size are the position of the Id in an SCC.
--
-- data Id = Id H.Hash Pos Size

data CodeInfo = CodeInfo
  { codeTermNames :: Relation Referent Name,
    codeTypeNames :: Relation Reference Name,
    -- | A combination of @codeTermNames@ and @codeTypeNames@,
    -- with the data constructors filtered out.
    codeDeclarationNames :: Relation Reference Name,
    -- | This @SyntaxText@ used to be @Text@,
    -- but got upgraded so that we can support clickable identifiers.
    codeBodies :: Map Reference SyntaxText,
    codeDependencies :: DependencyGraph,
    docBodies :: Map Reference [DisplayDoc.Element],
    -- | For debugging.
    showBodies :: Map Reference Text
  }

newtype DependencyGraph
  = -- | Relation from references to dependencies.
    DependencyGraph (Relation Reference Reference)
  deriving stock (Show, Generic)

shallowDependencies :: Reference -> DependencyGraph -> Set Reference
shallowDependencies ref (DependencyGraph deps) =
  Map.findWithDefault mempty ref (Relation.domain deps)

shallowReferences :: Reference -> DependencyGraph -> Set Reference
shallowReferences ref (DependencyGraph deps) =
  Map.findWithDefault mempty ref (Relation.range deps)

load :: String -> IO CodeInfo
load projectDirectory =
  loadCodeInfo Rt1.runtime =<< loadCodebaseAndBranch projectDirectory

loadCodeInfo :: Runtime.Runtime Symbol -> (Codebase IO Symbol Ann, Branch0 IO) -> IO CodeInfo
loadCodeInfo runtime (codebase, head) = do
  let terms :: Relation Referent Name
      terms =
        Branch.deepTerms head
      termsNoConstructors :: Relation Reference Name
      termsNoConstructors =
        mapMaybeRelation Referent.toTermReference terms
      types :: Relation Reference Name
      types =
        Branch.deepTypes head

  refsToBodies <- getBodies codebase head (Relation.domain termsNoConstructors) (Relation.domain types)
  refsToDocBodies <- getDocBodies codebase head runtime (Relation.domain termsNoConstructors)
  refsToShowBodies <- getShowBodies codebase (Relation.domain termsNoConstructors)

  callGraph <-
    functionCallGraph
      codebase
      (Map.keysSet (Relation.domain termsNoConstructors))
      (Map.keysSet (Relation.domain types))

  pure
    CodeInfo
      { codeTermNames = terms,
        codeTypeNames = types,
        codeDeclarationNames = termsNoConstructors <> types,
        codeBodies = refsToBodies,
        codeDependencies = callGraph,
        docBodies = refsToDocBodies,
        showBodies = refsToShowBodies
      }

-- | Ceremony around 'Term.dependencies' and 'Type.dependencies'.
functionCallGraph :: Codebase IO Symbol Ann -> Set Reference -> Set Reference -> IO DependencyGraph
functionCallGraph codebase terms types = do
  termDeps <- Map.fromList <$> for (Set.toList terms) termDependencies
  typeDeps <- Map.fromList <$> for (Set.toList types) typeDependencies
  pure (DependencyGraph (dropSelfEdges (Relation.fromMultimap (termDeps <> typeDeps))))
  where
    termDependencies :: Reference -> IO (Reference, Set Reference)
    termDependencies ref =
      case ref of
        Reference.Builtin _ ->
          pure (ref, mempty)
        Reference.DerivedId id -> do
          mTerm <- Codebase.getTerm codebase id
          case mTerm of
            Nothing -> do
              TIO.hPutStrLn System.IO.stderr ("Skipping reference (can't find term): " <> show id)
              pure (ref, mempty)
            Just (t :: Term Symbol Ann) ->
              pure (ref, Term.dependencies t)

    typeDependencies :: Reference -> IO (Reference, Set Reference)
    typeDependencies ref =
      case ref of
        Reference.Builtin _ ->
          pure (ref, mempty)
        Reference.DerivedId id -> do
          mType <- Codebase.getTypeDeclaration codebase id
          case mType of
            Nothing -> do
              TIO.hPutStrLn System.IO.stderr ("Skipping reference (can't find type): " <> show id)
              pure (ref, mempty)
            Just (t :: Decl Symbol Ann) ->
              pure (ref, Decl.declDependencies t)

    dropSelfEdges :: Ord a => Relation a a -> Relation a a
    dropSelfEdges =
      Relation.filter (uncurry (/=))

getBodies ::
  Codebase IO Symbol ann ->
  Branch0 IO ->
  Map Reference (Set Name) ->
  Map Reference (Set Name) ->
  IO (Map Reference SyntaxText)
getBodies codebase branch0 termMap typeMap = do
  termBodies <- Map.traverseWithKey (printTerm codebase branch0) termMap
  typeBodies <- Map.traverseWithKey (printType codebase branch0) typeMap
  pure (termBodies <> typeBodies)

getDocBodies ::
  Codebase IO Symbol Ann ->
  Branch0 IO ->
  Runtime.Runtime Symbol ->
  Map Reference (Set Name) ->
  IO (Map Reference [Element])
getDocBodies codebase branch0 runtime termMap = do
  Map.traverseMaybeWithKey (printDoc codebase branch0 runtime termMap) termMap

getShowBodies ::
  Codebase IO Symbol ann ->
  Map Reference (Set Name) ->
  IO (Map Reference Text)
getShowBodies codebase termMap = do
  Map.traverseWithKey (\a _ -> debugTerm codebase a) termMap

mapMaybeRelation ::
  forall a b c.
  (Ord b, Ord c) =>
  (a -> Maybe b) ->
  Relation a c ->
  Relation b c
mapMaybeRelation f =
  Relation.fromList . mapMaybe g . Relation.toList
  where
    g :: (a, c) -> Maybe (b, c)
    g (a, c) =
      (,c) <$> f a
