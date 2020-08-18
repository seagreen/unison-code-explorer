module UCE.Code.Print where

import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Text.JSON.Generic
import UCE.Prelude
import qualified Unison.ABT as ABT
import Unison.Codebase
import Unison.Codebase.Branch (Branch0 (..))
import qualified Unison.Codebase.Branch as Branch
import Unison.DataDeclaration (Decl)
import Unison.DeclPrinter
import Unison.HashQualified
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names3
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Reference (Id (..), Reference (..))
import qualified Unison.Reference
import qualified Unison.Referent
import qualified Unison.ShortHash
import Unison.Symbol (Symbol)
import qualified Unison.Term
import Unison.TermPrinter
import Unison.Util.AnnotatedText (AnnotatedText (..))
import Unison.Util.Pretty hiding (text, toPlain)
import Unison.Util.SyntaxText (SyntaxText, toPlain)
import qualified Unison.Util.SyntaxText as ST
import qualified UCE.Static.DisplayDoc as DisplayDoc
import qualified Unison.Builtin.Decls as DD
import qualified Unison.DataDeclaration as DD

data SegmentKind
  = WithHash {name :: String, hash :: String}
  | Other String
  | None
  deriving (Show, Data, Typeable)

data Segment = Segment
  { contents :: String,
    kind :: SegmentKind
  }
  deriving (Show, Data, Typeable)

elementToSegments :: Show ST.Element => (String, Maybe ST.Element) -> Segment
elementToSegments (text, element) = case element of
  Nothing -> Segment {contents = text, kind = None}
  Just el ->
    Segment
      { contents = text,
        kind = case el of
          ST.Reference r -> WithHash {hash = Unison.ShortHash.toString . Unison.Reference.toShortHash $ r, name = "Reference"}
          ST.Referent r -> WithHash {hash = Unison.ShortHash.toString . Unison.Referent.toShortHash $ r, name = "Referent"}
          ST.HashQualifier hq -> case (Unison.HashQualified.toHash hq) of
            Nothing -> Other "HashQualifier"
            Just hash -> WithHash {hash = Unison.ShortHash.toString hash, name = "HashQualifier"}
          _ -> Other (show el)
      }

toSegments :: SyntaxText -> [Segment]
toSegments (AnnotatedText items) =
  UCE.Prelude.map elementToSegments $ toList items

elementToHtml :: Show a => (String, Maybe a) -> String
elementToHtml (text, element) = case element of
  Nothing -> text
  Just el -> "<span class=\"" <> show el <> "\">" <> text <> "</span>"

toHtml :: SyntaxText -> String
toHtml (AnnotatedText items) =
  join $ UCE.Prelude.map elementToHtml $ toList items

getTermWithTypeAnnotation ::
  (Monad m, Ord v) =>
  Codebase m v ap ->
  Id ->
  m (Maybe (Unison.Term.Term v ap))
getTermWithTypeAnnotation codebase id = do
  mTerm <- getTerm codebase id
  case mTerm of
    Nothing -> pure Nothing
    Just term -> case term of
      -- if the term already has an annotation, leave it alone
      Unison.Term.Ann' _ _ -> pure $ Just term
      _ -> do
        mType <- getTypeOfTermImpl codebase id
        case mType of
          Nothing -> pure $ Just term
          Just typ -> pure $ Just (Unison.Term.ann (ABT.annotation term) term typ)

syntaxToPlain :: SyntaxText -> Text
syntaxToPlain = Text.pack . toPlain



termAsDoc term = case term of
  DD.DocJoin _ -> Just term
  DD.DocBlob _ -> Just term
  DD.DocLink _ -> Just term
  DD.DocSource _ -> Just term
  DD.DocSignature _ -> Just term
  DD.DocEvaluate _ -> Just term
  Unison.Term.Ann' inner _ -> termAsDoc inner
  _ -> Nothing

debugTerm codebase ref =
  case ref of
    Builtin _ -> pure "<builtin>"
    DerivedId id -> do
      mTerm <- getTerm codebase id
      pure $ show mTerm

printDoc ::
  Codebase IO Symbol ann ->
  Branch0 IO ->
  Map Reference (Set Name) ->
  Map Reference (Set Name) ->
  Reference ->
  Set Name ->
  IO (Maybe [DisplayDoc.Element])
printDoc codebase branch0 termMap typeMap ref nameSet =
  case ref of
    Builtin _ -> pure Nothing
    DerivedId id -> do
      mTerm <- getTerm codebase id
      case mTerm of
        Nothing -> pure Nothing
        Just term -> case (termAsDoc term) of
          Nothing -> pure Nothing
          Just doc -> do
            result <- (DisplayDoc.displayDoc showTypeSource showTermSource showSignature showResult doc)
            pure (Just result)
      -- mTerm <- getTermWithTypeAnnotation codebase id
      -- case mTerm of
      --   Nothing ->
      --     panic (show (name, id))
      --   Just term ->
      --     let pret :: Pretty SyntaxText
      --         pret =
      --           prettyBinding (printEnv branch0) (NameOnly name) term
      --      in pure $ render 80 pret
  where
    showTypeSource reference =
      printType codebase branch0 reference Set.empty
      -- pure (AnnotatedText (Seq.singleton ("type source", Nothing)))
    showTermSource referent = pure (AnnotatedText (Seq.singleton ("term source", Nothing)))
    showSignature _ = pure (AnnotatedText (Seq.singleton ("sig source", Nothing)))
    showResult _ = pure (AnnotatedText (Seq.singleton ("res source", Nothing)))
    -- name =
    --   case setToMaybe nameSet of
    --     Nothing -> Name.fromString "<name not found>"
    --     Just n -> n

printTerm ::
  Codebase IO Symbol ann ->
  Branch0 IO ->
  Reference ->
  Set Name ->
  IO SyntaxText
printTerm codebase branch0 ref nameSet =
  case ref of
    Builtin _ ->
      pure "<builtin>"
    DerivedId id -> do
      mTerm <- getTermWithTypeAnnotation codebase id
      case mTerm of
        Nothing ->
          panic (show (name, id))
        Just term ->
          let pret :: Pretty SyntaxText
              pret =
                prettyBinding (printEnv branch0) (NameOnly name) term
           in pure $ render 80 pret
  where
    name =
      case setToMaybe nameSet of
        Nothing -> Name.fromString "<name not found>"
        Just n -> n

printEnv :: Branch0 m -> PPE.PrettyPrintEnv
printEnv branch =
  PPE.fromNames 10 $ Names (Branch.toNames0 branch) mempty

printType ::
  Codebase IO Symbol ann ->
  Branch0 IO ->
  Reference ->
  Set Name ->
  IO SyntaxText
printType codebase branch0 ref nameSet =
  case ref of
    Builtin _ ->
      pure "<builtin type>"
    DerivedId id -> do
      mDecl <- getTypeDeclaration codebase id
      case mDecl of
        Nothing ->
          panic (show (name, id))
        Just (decl :: Decl Symbol ann) ->
          case decl of
            Left effectDecl ->
              let pret :: Pretty SyntaxText
                  pret =
                    prettyEffectDecl (printEnv branch0) ref (NameOnly name) effectDecl
               in pure $ render 80 pret
            Right dataDecl ->
              let pret :: Pretty SyntaxText
                  pret =
                    prettyDataDecl (printEnv branch0) ref (NameOnly name) dataDecl
               in pure $ render 80 pret
  where
    name =
      case setToMaybe nameSet of
        Nothing -> Name.fromString "<name not found>"
        Just n -> n
