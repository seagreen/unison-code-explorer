module Lib where

import Control.Monad
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable
import Prelude hiding (head, id)
import System.Exit (die)
import Unison.Codebase (Codebase)
import Unison.Codebase.Serialization.V0 (formatSymbol)
import Unison.Name (Name)
import Unison.Parser (Ann(External))
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Symbol (Symbol)
import Unison.Util.Relation (Relation)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
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
-- import qualified Unison.Codebase.Editor as Editor

-- * Interface

newtype Hash
  = Hash Text
  deriving (Eq, Ord, Show)

data Names
  = Names (Map Hash Text)
  deriving (Show)

data FunctionCallGraph
  = FunctionCallGraph (Map Hash (Set Hash))
  deriving (Show)

-- | A meaningless placeholder for now.
data Config
  = Config Bool String
  deriving (Show)

run :: Config -> IO ()
run conf = do
  (names, FunctionCallGraph functionCallGraph) <- load conf
  print names
  traverse print (Map.toList functionCallGraph)
  putStrLn "Success"

-- * Details

load :: Config -> IO (Names, FunctionCallGraph)
load _ = do
  let
    codebasePath :: FilePath
    codebasePath =
      ".unison"

    codebase :: Codebase IO Symbol Ann
    codebase =
      FileCodebase.codebase1 External formatSymbol formatAnn codebasePath

  exists <- FileCodebase.exists codebasePath
  when (not exists) (die "No codebase found")

  -- Editor.initializeCodebase codebase

  mBranch <- Codebase.getBranch codebase "master"
  branch <- case mBranch of
              Nothing ->
                die "getBranch failed"

              Just b ->
                pure b
  let
    head :: Branch.Branch0
    head = Branch.head branch

    terms :: Relation Name Referent
    terms = Branch.termNamespace head

    refToName :: Map Referent (Set Name)
    refToName = Relation.range terms

    refMap :: Map Referent Reference.Id
    refMap =
      Map.fromList $
        mapMaybe
          (\referent ->
            fmap
              (\id -> (referent, id))
              (r2r referent))
          (Map.keys refToName)

  res <- fcg codebase (Set.fromList (Map.elems refMap))
  pure (mkNames refMap refToName, res)

fcg :: Codebase IO Symbol Ann -> Set Reference.Id -> IO FunctionCallGraph
fcg codebase refs = do
  FunctionCallGraph . Map.fromList <$> for (Set.toList refs) f
  where
    f :: Reference.Id -> IO (Hash, Set Hash)
    f ref = do
      mTerm <- Codebase.getTerm codebase ref
      case mTerm of
        Nothing -> do
          putStrLn ("No term for reference: " <> show ref)
          pure (Hash (T.pack (show ref)), mempty)

        Just (t :: Codebase.Term Symbol Ann) ->
          pure (Hash (T.pack (show ref)), Set.map Hash (calls t))

mkNames :: Map Referent Reference.Id -> Map Referent (Set Name) -> Names
mkNames xs nameMap =
  Names (Map.fromList (fmap f (Map.toList xs)))
  where
    f :: (Referent, Reference.Id) -> (Hash, Text)
    f (referent, id) =
      case Map.lookup referent nameMap of
        Nothing ->
          error "Name not found"

        Just names ->
          ( Hash (T.pack (show id))
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

-- | @Codebase.Term Symbol Ann@ desugars to
-- @ABT.Term (Term.F Symbol Ann Ann) Symbol Ann@.
calls :: ABT.Term (Term.F Symbol Ann Ann) Symbol Ann -> Set Text
calls t =
  Set.map (T.pack . show) (Term.dependencies t)

r2r :: Referent -> Maybe Reference.Id
r2r r =
  case reference of
    Reference.Builtin _ ->
      Nothing

    Reference.DerivedId id ->
      Just id
  where
    reference :: Reference
    reference =
      case r of
        Referent.Ref a ->
          a

        Referent.Con a _ ->
          a

formatAnn :: S.Format Ann
formatAnn =
  S.Format (pure External) (\_ -> pure ())
