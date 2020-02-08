-- | Use the Unison compiler as a library to get info about a codebase.
module UCE.Code
  ( load
  , CodeInfo(..)
  , DependencyGraph(..)
  , shallowDependencies
  , shallowReferences
  , Name
  , Reference
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import System.IO (stderr)
import UCE.Code.Print
import UCE.Prelude
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch0(..))
import Unison.Codebase.Serialization.V1 (formatSymbol)
import Unison.DataDeclaration (Decl)
import Unison.Name (Name)
import Unison.Parser (Ann(External))
import Unison.Reference (Reference)
import Unison.Reference
import Unison.Referent (Referent)
import Unison.Referent
import Unison.Symbol (Symbol)
import Unison.Util.Relation (Relation)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.FileCodebase as FileCodebase
import qualified Unison.Codebase.Serialization as Serialization
import qualified Unison.DataDeclaration as Decl
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
  { codeTermNames :: Relation Referent Name
  , codeTypeNames :: Relation Reference Name

  , codeDeclarationNames :: Relation Reference Name
    -- ^ A combination of @codeTermNames@ and @codeTypeNames@,
    -- with the data constructors filtered out.

  , codeBodies :: Map Reference Text
  , codeDependencies :: DependencyGraph
  }

newtype DependencyGraph
  = DependencyGraph (Relation Reference Reference)
  -- ^ Relation from references to dependencies.
  deriving stock (Show, Generic)

shallowDependencies :: Reference -> DependencyGraph -> Set Reference
shallowDependencies ref (DependencyGraph deps) =
  Map.findWithDefault mempty ref (Relation.domain deps)

shallowReferences :: Reference -> DependencyGraph -> Set Reference
shallowReferences ref (DependencyGraph deps) =
  Map.findWithDefault mempty ref (Relation.range deps)

load :: IO CodeInfo
load =
  loadCodeInfo =<< loadCodebaseAndBranch

loadCodebaseAndBranch :: IO (Codebase IO Symbol Ann, Branch0 IO)
loadCodebaseAndBranch = do
  let
    codebasePath :: FilePath
    codebasePath =
      ".unison/v1"

    codebase :: Codebase IO Symbol Ann
    codebase =
      FileCodebase.codebase1 formatSymbol formatAnn codebasePath

  exists <- FileCodebase.exists codebasePath
  when (not exists) (die "No codebase found")

  branch <- Codebase.getRootBranch codebase

  let
    head :: Branch0 IO
    head =
      Branch.head branch

  pure (codebase, head)
  where
    formatAnn :: Serialization.Format Ann
    formatAnn =
      Serialization.Format (pure External) (\_ -> pure ())

loadCodeInfo :: (Codebase IO Symbol Ann, Branch0 IO) -> IO CodeInfo
loadCodeInfo (codebase, head) = do
  let
    terms :: Relation Referent Name
    terms =
      Branch.deepTerms head

    termsNoConstructors :: Relation Reference Name
    termsNoConstructors =
      mapMaybeRelation referentToRef terms

    types :: Relation Reference Name
    types =
      Branch.deepTypes head

  refsToBodies <- getBodies codebase head (Relation.domain termsNoConstructors) (Relation.domain types)

  callGraph <-
    functionCallGraph
      codebase
      (Map.keysSet (Relation.domain termsNoConstructors))
      (Map.keysSet (Relation.domain types))

  pure CodeInfo
    { codeTermNames        = terms
    , codeTypeNames        = types
    , codeDeclarationNames = termsNoConstructors <> types
    , codeBodies           = refsToBodies
    , codeDependencies     = callGraph
    }

-- | Ceremony around 'Term.dependencies' and 'Type.dependencies'.
functionCallGraph :: Codebase IO Symbol Ann -> Set Reference -> Set Reference -> IO DependencyGraph
functionCallGraph codebase terms types = do
  termDeps <- Map.fromList <$> for (Set.toList terms) termDependencies
  typeDeps <- Map.fromList <$> for (Set.toList types) typeDependencies
  pure (DependencyGraph (Relation.fromMultimap (termDeps <> typeDeps)))
  where
    termDependencies :: Reference -> IO (Reference, Set Reference)
    termDependencies ref =
      case ref of
        Builtin _ ->
          pure (ref, mempty)

        DerivedId id -> do
          mTerm <- Codebase.getTerm codebase id
          case mTerm of
            Nothing -> do
              TIO.hPutStrLn System.IO.stderr ("Skipping reference (can't find term): " <> showText id)
              pure (ref, mempty)

            Just (t :: Codebase.Term Symbol Ann) ->
              pure (ref, Term.dependencies t)

    typeDependencies :: Reference -> IO (Reference, Set Reference)
    typeDependencies ref =
      case ref of
        Builtin _ ->
          pure (ref, mempty)

        DerivedId id -> do
          mType <- Codebase.getTypeDeclaration codebase id
          case mType of
            Nothing -> do
              TIO.hPutStrLn System.IO.stderr ("Skipping reference (can't find type): " <> showText id)
              pure (ref, mempty)

            Just (t :: Decl Symbol Ann) ->
              pure (ref, Decl.declDependencies t)

getBodies
  :: Codebase IO Symbol ann
  -> Branch0 IO
  -> Map Reference (Set Name)
  -> Map Reference (Set Name)
  -> IO (Map Reference Text)
getBodies codebase branch0 termMap typeMap = do
  termBodies <- Map.traverseWithKey (printTerm codebase branch0) termMap
  typeBodies <- Map.traverseWithKey (printType codebase branch0) typeMap
  pure (termBodies <> typeBodies)

mapMaybeRelation
  :: forall a b c. (Ord b, Ord c)
  => (a -> Maybe b)
  -> Relation a c
  -> Relation b c
mapMaybeRelation f =
  Relation.fromList . mapMaybe g . Relation.toList
  where
    g :: (a, c) -> Maybe (b, c)
    g (a, c) =
      (,c) <$> f a

referentToRef :: Referent -> Maybe Reference
referentToRef referent =
  case referent of
    Con{} ->
      Nothing

    Ref ref ->
      Just ref
