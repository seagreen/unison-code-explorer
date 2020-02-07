-- | Use the Unison compiler as a library to get info about a codebase.
module UCE.CodeInfo
  ( load
  , CodeInfo(..)
  , FunctionCallGraph(..)
  , Hash(..)
  , Name
  , Reference
  ) where

import Data.Map (Map)
import Data.Text (Text)
import System.IO (stderr)
import UCE.Prelude
import Unison.Codebase (Codebase)
import Unison.Codebase
import Unison.Codebase.Branch (Branch0(..))
import Unison.Codebase.Serialization.V1 (formatSymbol)
import Unison.HashQualified
import Unison.Name (Name)
import Unison.Names3
import Unison.Parser (Ann(External))
import Unison.Reference (Reference)
import Unison.Reference
import Unison.Referent (Referent)
import Unison.Referent
import Unison.Symbol (Symbol)
import Unison.TermPrinter
import Unison.Util.Pretty hiding (toPlain)
import Unison.Util.Relation (Relation)
import Unison.Util.SyntaxText

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Unison.ABT as ABT
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.FileCodebase as FileCodebase
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Name as Name
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
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
  { apiNames :: Map Name (Set Reference)
    -- ^ Invariant: @Set Hash@ is nonempty.

  , termBodies :: Map Reference Text
  , apiFcg :: FunctionCallGraph
  }

newtype Hash
  = Hash { unHash :: Text }
  deriving stock (Eq, Ord, Show, Generic)

data FunctionCallGraph
  = FunctionCallGraph (Map Hash (Set Hash))
  deriving stock (Show, Generic)

load :: IO CodeInfo
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
    head :: Branch0 IO
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

  termBodies <- getTerms codebase head
  callGraph <- fcg codebase (Set.fromList (Map.elems refToId))
  pure (CodeInfo (mkNames refToId refToName) termBodies callGraph)

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

mkNames :: Map Referent Reference.Id -> Map Referent (Set Name) -> Map Name (Set Reference)
mkNames xs nameMap =
  Map.mapMaybe f (swapMap nameMap)
  where
    f :: Set Referent -> Maybe (Set Reference)
    f referents =
      case mapMaybe g (Set.toList referents) of
        [] ->
          Nothing

        zs ->
          Just (Set.fromList zs)

    g :: Referent -> Maybe Reference
    g = \case
      Con{} ->
        Nothing

      Ref ref ->
        Just ref

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

getTerms :: Codebase IO Symbol ann -> Branch0 IO -> IO (Map Reference Text)
getTerms codebase branch0 =
  let
    -- Referent: reference to a term
    terms :: Relation Referent Name
    terms =
      Branch.deepTerms branch0

    termMap :: Map Reference (Set Name)
    termMap =
      Map.fromList $ mapMaybe filterOutConstructors (Map.toList (Relation.domain terms))
      where
        filterOutConstructors :: (Referent, a) -> Maybe (Reference, a)
        filterOutConstructors (referent, a) =
          case referent of
            Ref ref ->
              Just (ref, a)

            Con{} ->
              Nothing
  in
    Map.traverseWithKey (printTerm codebase branch0) termMap

printTerm
  :: Codebase IO Symbol ann
  -> Branch0 IO
  -> Reference
  -> Set Name
  -> IO Text
printTerm codebase branch0 ref nameSet =
  case ref of
    Unison.Reference.Builtin _ ->
      pure "<builtin>"

    DerivedId id -> do
      mTerm <- getTerm codebase id
      case mTerm of
        Nothing -> do
          putStrLn "printTerm 1"
          panic (showText (name, id))

        Just term ->
          let
            pret :: Pretty SyntaxText
            pret =
              prettyBinding (printEnv branch0) (NameOnly name) term
          in
            pure . Text.pack . toPlain $ render 80 pret
  where
    name =
      case setToMaybe nameSet of
        Nothing -> Name.fromString "<name not found>"
        Just n -> n

printEnv :: Branch0 m -> PPE.PrettyPrintEnv
printEnv branch =
  PPE.fromNames 10 $ Names (Branch.toNames0 branch) mempty
