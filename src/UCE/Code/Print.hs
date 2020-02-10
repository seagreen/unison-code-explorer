module UCE.Code.Print where

import Data.Text (Text)
import UCE.Prelude
import Unison.Codebase (Codebase)
import Unison.Codebase
import Unison.Codebase.Branch (Branch0(..))
import Unison.DataDeclaration (Decl)
import Unison.DeclPrinter
import Unison.HashQualified
import Unison.Name (Name)
import Unison.Names3
import Unison.Reference (Reference(..))
import Unison.Symbol (Symbol)
import Unison.TermPrinter
import Unison.Util.Pretty hiding (toPlain)
import Unison.Util.SyntaxText

import qualified Data.Text as Text
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Name as Name
import qualified Unison.PrettyPrintEnv as PPE

printTerm
  :: Codebase IO Symbol ann
  -> Branch0 IO
  -> Reference
  -> Set Name
  -> IO Text
printTerm codebase branch0 ref nameSet =
  case ref of
    Builtin _ ->
      pure "<builtin>"

    DerivedId id -> do
      mTerm <- getTerm codebase id
      case mTerm of
        Nothing ->
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

printType
  :: Codebase IO Symbol ann
  -> Branch0 IO
  -> Reference
  -> Set Name
  -> IO Text
printType codebase branch0 ref nameSet =
  case ref of
    Builtin _ ->
      pure "<builtin type>"

    DerivedId id -> do
      mDecl <- getTypeDeclaration codebase id
      case mDecl of
        Nothing ->
          panic (showText (name, id))

        Just (decl :: Decl Symbol ann) ->
          case decl of
            Left effectDecl ->
              let
                pret :: Pretty SyntaxText
                pret =
                  prettyEffectDecl (printEnv branch0) ref (NameOnly name) effectDecl
              in
                pure . Text.pack . toPlain $ render 80 pret

            Right dataDecl ->
              let
                pret :: Pretty SyntaxText
                pret =
                  prettyDataDecl (printEnv branch0) ref (NameOnly name) dataDecl
              in
                pure . Text.pack . toPlain $ render 80 pret
  where
    name =
      case setToMaybe nameSet of
        Nothing -> Name.fromString "<name not found>"
        Just n -> n
