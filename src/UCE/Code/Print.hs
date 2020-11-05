module UCE.Code.Print
  ( Segment (..),
    printDoc,
    printTerm,
    printType,
    toSegments,
    debugTerm,
  )
where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Text.JSON.Generic
import UCE.Prelude
import qualified UCE.Static.DisplayDoc as DisplayDoc
import qualified Unison.ABT as ABT
import qualified Unison.Builtin.Decls as DD
import Unison.Codebase
import Unison.Codebase.Branch (Branch0 (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Runtime as Runtime
import Unison.DataDeclaration (Decl)
import Unison.DeclPrinter
import Unison.HashQualified
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names3
import qualified Unison.Parser
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Reference (Id (..), Reference (..))
import qualified Unison.Reference
import qualified Unison.Referent
import qualified Unison.ShortHash
import Unison.Symbol (Symbol)
import qualified Unison.Term
import Unison.TermPrinter
import qualified Unison.TypePrinter as TypePrinter
import Unison.Util.AnnotatedText (AnnotatedText (..))
import Unison.Util.Pretty hiding (text, toPlain)
import Unison.Util.SyntaxText (SyntaxText)
import qualified Unison.Util.SyntaxText as ST

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

elementToSegments :: (String, Maybe ST.Element) -> Segment
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

termAsDoc term = case term of
  DD.DocJoin _ -> Just term
  DD.DocBlob _ -> Just term
  DD.DocLink _ -> Just term
  DD.DocSource _ -> Just term
  DD.DocSignature _ -> Just term
  DD.DocEvaluate _ -> Just term
  Unison.Term.Ann' inner _ -> termAsDoc inner
  -- Unison.Term.Ref' _ -> Just term  -- @[include]
  _ -> Nothing

debugTerm codebase ref =
  case ref of
    Builtin _ -> pure "<builtin>"
    DerivedId id -> do
      mTerm <- getTerm codebase id
      pure $ show mTerm

getOrDie map k = case Map.lookup k map of
  Nothing -> pure (Set.empty)
  Just m -> pure m

printDoc codebase branch0 runtime termMap ref nameSet =
  case ref of
    Builtin _ -> pure Nothing
    DerivedId id -> do
      mTerm <- getTerm codebase id
      case mTerm of
        Nothing -> pure Nothing
        Just term -> case (termAsDoc term) of
          Nothing -> pure Nothing
          Just doc -> do
            result <- (DisplayDoc.displayDoc showTypeSource showTermSource showSignature showResult getInclude doc)
            pure (Just result)
  where
    -- mTerm <- getTermWithTypeAnnotation codebase id
    -- case mTerm of
    --   Nothing ->
    --     panic (show (name, id))
    --   Just term ->
    --     let pret :: Pretty SyntaxText
    --         pret =
    --           prettyBinding (printEnv branch0) (NameOnly name) term
    --      in pure $ render 80 pret

    showTypeSource reference = do
      names <- getOrDie termMap reference
      printType codebase branch0 reference names
    -- pure (AnnotatedText (Seq.singleton ("type source", Nothing)))

    -- showTermSource referent = do
    --   names <- getOrDie typeMap (Unison.Referent.toReference referent)
    --   printTerm codebase branch0 (Unison.Referent.toReference referent) names
    --   -- pure (AnnotatedText (Seq.singleton ("term source", Nothing)))
    showTermSource r = case r of
      Unison.Referent.Ref (Unison.Reference.Builtin n) -> pure (AnnotatedText (Seq.singleton (n & Text.unpack, Nothing)))
      Unison.Referent.Ref ref' -> do
        names <- getOrDie termMap ref'
        printTerm codebase branch0 ref' names
      Unison.Referent.Con r' _ _ -> do
        names <- getOrDie termMap r'
        printTerm codebase branch0 r' names

    showSignature r = do
      let x = Unison.Codebase.getTypeOfTerm codebase (Unison.Referent.toReference r)
      typeOf <- x
      case typeOf of
        Nothing -> pure (AnnotatedText (Seq.singleton ("No signature found", Nothing)))
        Just t ->
          pure $
            render 80 $
              TypePrinter.pretty0 (printEnv branch0) Map.empty (-1) t

    getResult id = do
      term <- getTerm codebase id
      case term of
        Nothing -> pure Nothing
        Just term' -> do
          result <- eval1 runtime codebase (printEnv branch0) term'
          pure (Just result)

    getInclude r = case r of
      (Unison.Reference.DerivedId id) -> do
        evaluated <- getResult id
        pure $ case evaluated of
          Nothing -> Nothing
          Just (Left _) -> Nothing
          Just (Right included) -> Just included
      _ -> pure Nothing

    showResult r = case r of
      Unison.Referent.Ref (Unison.Reference.Builtin n) -> pure (AnnotatedText (Seq.singleton (n & Text.unpack, Nothing)))
      Unison.Referent.Ref (Unison.Reference.DerivedId id) -> do
        term <- getTerm codebase id
        case term of
          Nothing -> pure (AnnotatedText (Seq.singleton ("Term not found", Nothing)))
          Just term' -> do
            result <- eval1 runtime codebase (printEnv branch0) term'
            case result of
              Left error' -> pure (AnnotatedText (Seq.singleton (show error', Nothing)))
              Right term'' ->
                let pret :: Pretty SyntaxText
                    pret =
                      let term''' = printAnnotate (printEnv branch0) term''
                       in let (im', _uses) = calcImports Map.empty term'''
                           in pretty0 (printEnv branch0) (ac 0 Block im' MaybeDoc) term'''
                 in -- prettyBinding (printEnv branch0) (NameOnly "EVAL") term''
                    pure $ render 80 pret
      -- names <- getOrDie termMap ref'
      Unison.Referent.Con r' _ _ -> do
        names <- getOrDie termMap r'
        printTerm codebase branch0 r' names

-- let codeLookup = Codebase.toCodeLookup codebase
-- r <- Runtime.evaluateTerm codeLookup ppe rt tm
-- pure $ r <&> Term.amap (const Parser.External)

-- eval1 :: PPE.PrettyPrintEnv -> Term v Ann -> _
eval1 runtime codebase ppe tm = do
  let codeLookup = Unison.Codebase.toCodeLookup codebase
  r <- Runtime.evaluateTerm codeLookup ppe runtime tm
  pure $ r <&> Unison.Term.amap (const Unison.Parser.External)

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
