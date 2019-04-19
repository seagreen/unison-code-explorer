module Lib where

import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Prelude hiding (head, id)
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

newtype Hash = Hash Text

data FunctionCallGraph = FunctionCallGraph (Map Hash (Set Hash))

data Names = Names (Map Hash Text)

data Config = Config Bool String deriving Show

run :: Config -> IO ()
run _ = do
  let
    codebasePath :: FilePath
    codebasePath =
      ".unison"

    codebase :: Codebase IO Symbol Ann
    codebase =
      FileCodebase.codebase1 External formatSymbol formatAnn codebasePath

  exists <- FileCodebase.exists codebasePath
  if exists
    then do
      -- Editor.initializeCodebase codebase
      branches <- Codebase.branches codebase
      print branches

      mBranch <- Codebase.getBranch codebase "master"
      case mBranch of
        Nothing ->
          putStrLn "getBranch failed"

        Just branch -> do
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
              putStrLn "No term"

            Just (t :: Codebase.Term Symbol Ann) -> do
              print (Map.lookup r1 refToName)
              print (calls t)
              putStrLn "Success"
    else
      putStrLn "No codebase found"

formatAnn :: S.Format Ann
formatAnn =
  S.Format (pure External) (\_ -> pure ())

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
