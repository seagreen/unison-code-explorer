module UCE.Code.Print where

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
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.FileCodebase as FileCodebase
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Name as Name
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Term as Term
import qualified Unison.Util.Relation as Relation

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
