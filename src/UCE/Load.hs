-- | Use the Unison compiler as a library to get info about a codebase.
module UCE.Load
  ( load
  , API(..)
  , FunctionCallGraph(..)
  , Names(..)
  , Hash(..)
  ) where

import Data.Aeson
import UCE.Prelude
import Unison.Codebase (Codebase)
import Unison.Codebase.Serialization.V1 (formatSymbol)
import Unison.Name (Name)
import Unison.Parser (Ann(External))
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Symbol (Symbol)
import Unison.Util.Relation (Relation)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO
import qualified Unison.ABT as ABT
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.FileCodebase as FileCodebase
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Name as Name
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Term as Term
import qualified Unison.Util.Relation as Relation

data API = API
  { apiNames :: Names
  , apiFcg :: FunctionCallGraph
  }

data FunctionCallGraph
  = FunctionCallGraph (Map Hash (Set Hash))
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Names
  = Names (Map Hash Text)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype Hash
  = Hash Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToJSONKey)

load :: IO API
load = do
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
    head :: Branch.Branch0 IO
    head =
      Branch.head branch

    terms :: Relation Referent Name
    terms =
      Branch.deepTerms head

    refToName :: Map Referent (Set Name)
    refToName =
      Relation.domain terms

    -- Start with @refToName@, filter out builtins with @referenceToId@,
    -- and replace the values with @Reference.Id@s.
    --
    -- We can't go ahead and turn these @Reference.Id@s into @Hash@es,
    -- because @fcg@ needs @Reference.Id@s to pass to @Term.dependencies@.
    refToId :: Map Referent Reference.Id
    refToId =
      Map.fromList $ mapMaybe f (Map.keys refToName)
      where
        f :: Referent -> Maybe (Referent, Reference.Id)
        f referent = do
          ref <- Referent.toTermReference referent
          id <- referenceToId ref
          pure (referent, id)

  callGraph <- fcg codebase (Set.fromList (Map.elems refToId))
  pure (API (mkNames refToId refToName) callGraph)

-- * Helpers

-- | A lot of ceremony around 'Term.dependencies'.
fcg :: Codebase IO Symbol Ann -> Set Reference.Id -> IO FunctionCallGraph
fcg codebase refIds = do
  FunctionCallGraph . Map.fromList <$> for (Set.toList refIds) f
  where
    f :: Reference.Id -> IO (Hash, Set Hash)
    f id = do
      mTerm <- Codebase.getTerm codebase id
      case mTerm of
        Nothing -> do
          TIO.hPutStrLn System.IO.stderr ("Skipping reference (can't find term): " <> idToHashText id)
          pure (idToHash id, mempty)

        Just (t :: Codebase.Term Symbol Ann) ->
          pure (idToHash id, calls t)

-- | @Codebase.Term Symbol Ann@ desugars to
-- @ABT.Term (Term.F Symbol Ann Ann) Symbol Ann@.
calls :: ABT.Term (Term.F Symbol Ann Ann) Symbol Ann -> Set Hash
calls =
  Set.fromList . mapMaybe f . Set.toList . Term.dependencies
  where
    f :: Reference -> Maybe Hash
    f =
      fmap idToHash . referenceToId

idToHash :: Reference.Id -> Hash
idToHash =
  Hash . idToHashText

-- | A separate function from 'idToHash' for use in making logs.
idToHashText :: Reference.Id -> Text
idToHashText (Reference.Id hash _ _) =
  T.pack (show hash)

mkNames :: Map Referent Reference.Id -> Map Referent (Set Name) -> Names
mkNames xs nameMap =
  Names (Map.fromList (fmap f (Map.toList xs)))
  where
    f :: (Referent, Reference.Id) -> (Hash, Text)
    f (referent, id) =
      case Map.lookup referent nameMap of
        Nothing ->
          panic "Name not found"

        Just names ->
          ( idToHash id
          , textFromName names
          )

textFromName :: Set Name -> Text
textFromName xs =
  case Set.toList xs of
    [] ->
      "<none>"

    [x] ->
      Name.toText x

    x:_ ->
      Name.toText x <> " (conflicted)"

-- | Filters out builtins.
referenceToId :: Reference -> Maybe Reference.Id
referenceToId ref =
  case ref of
    Reference.Builtin _ ->
      Nothing

    Reference.DerivedId id ->
      Just id

formatAnn :: S.Format Ann
formatAnn =
  S.Format (pure External) (\_ -> pure ())
