-- | Use the Unison compiler as a library to get info about a codebase.
module UCE.CodeInfo
  ( load
  , CodeInfo(..)
  , FunctionCallGraph(..)
  , functionCalls
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
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.FileCodebase as FileCodebase
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Name as Name
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as Reference
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

newtype FunctionCallGraph
  = FunctionCallGraph (Map Reference (Set Reference))
  deriving stock (Show, Generic)

functionCalls :: Reference -> FunctionCallGraph -> Set Reference
functionCalls ref (FunctionCallGraph fcg) =
  Map.findWithDefault mempty ref fcg

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

    referentToName :: Map Referent (Set Name)
    referentToName =
      Relation.domain terms

    refToName :: Map Reference (Set Name)
    refToName =
      dropConstructors referentToName

  termBodies <- getTerms codebase head --todo
  callGraph <- functionCallGraph codebase (Map.keysSet refToName)
  pure (CodeInfo (mkNames referentToName) termBodies callGraph)

-- * Helpers

-- | A lot of ceremony around 'Term.dependencies'.
functionCallGraph :: Codebase IO Symbol Ann -> Set Reference -> IO FunctionCallGraph
functionCallGraph codebase refs = do
  FunctionCallGraph . Map.fromList <$> for (Set.toList refs) f
  where
    f :: Reference -> IO (Reference, Set Reference)
    f ref =
      case ref of
        Builtin _ ->
          pure (ref, mempty)

        DerivedId id -> do
          mTerm <- Codebase.getTerm codebase id
          case mTerm of
            Nothing -> do
              TIO.hPutStrLn System.IO.stderr ("Skipping reference (can't find term): " <> idToHashText id)
              pure (ref, mempty)

            Just (t :: Codebase.Term Symbol Ann) ->
              pure (ref, Term.dependencies t)

-- | A separate function from 'idToHash' for use in making logs.
idToHashText :: Reference.Id -> Text
idToHashText (Reference.Id hash _ _) =
  T.pack (show hash)

mkNames :: Map Referent (Set Name) -> Map Name (Set Reference)
mkNames nameMap =
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

dropConstructors :: Map Referent a -> Map Reference a
dropConstructors referentMap =
  mapMaybeKey f referentMap
  where
    f :: Referent -> Maybe Reference
    f referent =
      case referent of
        Con{} ->
          Nothing

        Ref ref ->
          Just ref

    mapMaybeKey :: forall k x a. Ord x => (k -> Maybe x) -> Map k a -> Map x a
    mapMaybeKey fk xs =
      let
        g :: (k, a) -> Maybe (x, a)
        g (k, a) =
          (,a) <$> fk k
      in
        Map.fromList . mapMaybe g . Map.toList $ xs

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
