module Lib where

import Control.Monad
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
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
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Term as Term
import qualified Unison.Util.Relation as Relation
-- import qualified Unison.Codebase.Editor as Editor

-- * Interface

newtype Hash = Hash Text

data FunctionCallGraph = FunctionCallGraph (Map Hash (Set Hash))

data Names = Names (Map Hash Text)

-- | A meaningless placeholder for now.
data Config = Config Bool String deriving Show

run :: Config -> IO ()
run conf = do
  res <- load conf
  print res

-- * Details

load :: Config -> IO (Set Text, Maybe (Set Name))
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

    _nameToRef :: Map Name (Set Referent)
    _nameToRef = Relation.domain terms

    refToName :: Map Referent (Set Name)
    refToName = Relation.range terms

    refList :: [(Referent, Reference.Id)]
    refList =
      mapMaybe (\x -> fmap (\y -> (x, y)) (r2r x)) (Map.keys refToName)

    r1 :: Referent
    r2 :: Reference.Id
    (r1, r2) =
      let x:_ = refList
      in x

  mTerm <- Codebase.getTerm codebase r2
  case mTerm of
    Nothing ->
      die "No term"

    Just (t :: Codebase.Term Symbol Ann) -> do
      putStrLn "Success"
      pure (calls t, Map.lookup r1 refToName)

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
