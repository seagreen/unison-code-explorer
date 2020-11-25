module UCE.Code.Print
  ( printDoc,
    printTerm,
    printType,
    debugTerm,
  )
where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import UCE.Prelude
import qualified UCE.Static.DisplayDoc as DisplayDoc
import qualified Unison.ABT as ABT
import qualified Unison.Builtin.Decls as DD
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Runtime as Runtime
import Unison.DataDeclaration (Decl)
import Unison.DeclPrinter (prettyDataDecl, prettyEffectDecl)
import qualified Unison.HashQualified as HashQualified
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names3 (Names (..))
import Unison.Parser (Ann)
import qualified Unison.Parser
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Reference (Id)
import qualified Unison.Reference as Reference
import qualified Unison.Referent
import qualified Unison.Term
import Unison.TermPrinter
import qualified Unison.TypePrinter as TypePrinter
import qualified Unison.Util.Pretty as Pretty
import qualified Unison.Var

getTermWithTypeAnnotation ::
  (Monad m, Ord v) =>
  Codebase m v ap ->
  Id ->
  m (Maybe (Unison.Term.Term v ap))
getTermWithTypeAnnotation codebase id = do
  mTerm <- Codebase.getTerm codebase id
  case mTerm of
    Nothing -> pure Nothing
    Just term -> case term of
      -- if the term already has an annotation, leave it alone
      Unison.Term.Ann' _ _ -> pure $ Just term
      _ -> do
        mType <- Codebase.getTypeOfTermImpl codebase id
        case mType of
          Nothing -> pure $ Just term
          Just typ -> pure $ Just (Unison.Term.ann (ABT.annotation term) term typ)

termAsDoc :: Term v a -> Maybe (Term v a)
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

debugTerm :: (IsString a1, Monad f, Show v) => Codebase f v a2 -> Reference -> f a1
debugTerm codebase ref =
  case ref of
    Reference.Builtin _ -> pure "<builtin>"
    Reference.DerivedId id -> do
      mTerm <- Codebase.getTerm codebase id
      pure $ show mTerm

getOrDie :: (Ord k, Applicative f) => Map k (Set a) -> k -> f (Set a)
getOrDie map' k =
  case Map.lookup k map' of
    Nothing -> pure Set.empty
    Just m -> pure m

printDoc ::
  Codebase IO Symbol Ann ->
  Branch0 IO ->
  Runtime Symbol ->
  Map Reference (Set Name) ->
  Reference ->
  a ->
  IO (Maybe [DisplayDoc.Element])
printDoc codebase branch0 runtime termMap ref _nameSet =
  case ref of
    Reference.Builtin _ ->
      pure Nothing
    Reference.DerivedId id -> do
      mTerm <- Codebase.getTerm codebase id
      case mTerm of
        Nothing ->
          pure Nothing
        Just term ->
          case termAsDoc term of
            Nothing ->
              pure Nothing
            Just doc -> do
              result <- DisplayDoc.displayDoc showTypeSource showTermSource showSignature showResult getInclude doc
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
      Unison.Referent.Ref (Reference.Builtin n) -> pure (AnnotatedText (Seq.singleton (n & Text.unpack, Nothing)))
      Unison.Referent.Ref ref' -> do
        names <- getOrDie termMap ref'
        printTerm codebase branch0 ref' names
      Unison.Referent.Con r' _ _ -> do
        names <- getOrDie termMap r'
        printTerm codebase branch0 r' names

    showSignature r = do
      let x = Codebase.getTypeOfTerm codebase (Unison.Referent.toReference r)
      typeOf <- x
      case typeOf of
        Nothing -> pure (AnnotatedText (Seq.singleton ("No signature found", Nothing)))
        Just t ->
          pure $
            Pretty.render 80 $
              TypePrinter.pretty0 (printEnv branch0) Map.empty (-1) t

    getResult id = do
      term <- Codebase.getTerm codebase id
      case term of
        Nothing -> pure Nothing
        Just term' -> do
          result <- eval1 runtime codebase (printEnv branch0) term'
          pure (Just result)

    getInclude r = case r of
      (Reference.DerivedId id) -> do
        evaluated <- getResult id
        pure $ case evaluated of
          Nothing -> Nothing
          Just (Left _) -> Nothing
          Just (Right included) -> Just included
      _ -> pure Nothing

    showResult r = case r of
      Unison.Referent.Ref (Reference.Builtin n) -> pure (AnnotatedText (Seq.singleton (n & Text.unpack, Nothing)))
      Unison.Referent.Ref (Reference.DerivedId id) -> do
        term <- Codebase.getTerm codebase id
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
                    pure $ Pretty.render 80 pret
      -- names <- getOrDie termMap ref'
      Unison.Referent.Con r' _ _ -> do
        names <- getOrDie termMap r'
        printTerm codebase branch0 r' names

-- let codeLookup = Codebase.toCodeLookup codebase
-- r <- Runtime.evaluateTerm codeLookup ppe rt tm
-- pure $ r <&> Term.amap (const Parser.External)

-- eval1 :: PPE.PrettyPrintEnv -> Term v Ann -> _
eval1 ::
  (Unison.Var.Var v, Monoid a) =>
  Runtime v ->
  Codebase IO v a ->
  PPE.PrettyPrintEnv ->
  Term v a ->
  IO (Either Runtime.Error (Term v Ann))
eval1 runtime codebase ppe tm = do
  let codeLookup = Codebase.toCodeLookup codebase
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
    Reference.Builtin _ ->
      pure "<builtin>"
    Reference.DerivedId id -> do
      mTerm <- getTermWithTypeAnnotation codebase id
      case mTerm of
        Nothing ->
          panic (show (name, id))
        Just term ->
          let pret :: Pretty SyntaxText
              pret =
                prettyBinding (printEnv branch0) (HashQualified.NameOnly name) term
           in pure $ Pretty.render 80 pret
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
    Reference.Builtin _ ->
      pure "<builtin type>"
    Reference.DerivedId id -> do
      mDecl <- Codebase.getTypeDeclaration codebase id
      case mDecl of
        Nothing ->
          panic (show (name, id))
        Just (decl :: Decl Symbol ann) ->
          case decl of
            Left effectDecl ->
              let pret :: Pretty SyntaxText
                  pret =
                    prettyEffectDecl (printEnv branch0) ref (HashQualified.NameOnly name) effectDecl
               in pure $ Pretty.render 80 pret
            Right dataDecl ->
              let pret :: Pretty SyntaxText
                  pret =
                    prettyDataDecl (printEnv branch0) ref (HashQualified.NameOnly name) dataDecl
               in pure $ Pretty.render 80 pret
  where
    name =
      case setToMaybe nameSet of
        Nothing -> Name.fromString "<name not found>"
        Just n -> n
