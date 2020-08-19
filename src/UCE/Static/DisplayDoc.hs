{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module UCE.Static.DisplayDoc where

-- import Data.Foldable ( fold )

-- import Unison.Reference (Reference)
-- import Unison.Referent (Referent)
-- import Unison.Term (Term)
-- import Unison.Type (Type)
-- import Unison.Var (Var)

-- import Unison.Util.AnnotatedText      ( AnnotatedText(..), annotate )
import qualified Data.List as List
import Data.Text (pack, unpack)
import qualified Data.Text
import UCE.Prelude
import UCE.Static.Utils
import qualified Unison.Builtin.Decls as DD
import qualified Unison.DataDeclaration as DD
import qualified Unison.DeclPrinter as DP
import qualified Unison.NamePrinter as NP
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Term as Term
import qualified Unison.TermPrinter as TP
import qualified Unison.TypePrinter as TypePrinter
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.SyntaxText as S

type Pretty = P.Pretty P.ColorText

data Element
  = Text Text
  | TermLink Referent.Referent
  | TypeLink Reference.Reference
  | TermSource S.SyntaxText
  | TypeSource S.SyntaxText
  | Eval S.SyntaxText
  | Signature S.SyntaxText

-- displayTerm pped terms typeOf eval types tm = case tm of
--   Term.Ref' r -> eval r >>= \case
--     Nothing -> pure $ termName (PPE.suffixifiedPPE pped) (Referent.Ref r)
--     Just tm -> displayDoc pped terms typeOf eval types tm
--   _ -> displayDoc pped terms typeOf eval types tm

displayDoc showTypeSource showTermSource showSignature showResult = go
  where
    go (DD.DocJoin docs) = fold <$> traverse go docs
    -- go (DD.DocJoin docs) = docs & toList & map go & List.concat
    go (DD.DocBlob txt) = pure [txt & Text]
    go (DD.DocLink (DD.LinkTerm (Term.TermLink' r))) =
      pure [TermLink r]
    go (DD.DocLink (DD.LinkType (Term.TypeLink' r))) =
      pure [TypeLink r]
    go (DD.DocSource (DD.LinkTerm (Term.TermLink' r))) = do
      source <- (showTermSource r)
      pure [TermSource source]
    go (DD.DocSource (DD.LinkType (Term.TypeLink' r))) = do
      source <- (showTypeSource r)
      pure [TypeSource source]
    go (DD.DocSignature (Term.TermLink' r)) = do
      sig <- (showSignature r)
      pure [Signature sig]
    go (DD.DocEvaluate (Term.TermLink' r)) = do
      res <- (showResult r)
      pure [Eval res]
    go tm = pure [Text (show tm)]

--   prettySignature r = typeOf r >>= \case
--     Nothing -> pure $ termName (PPE.unsuffixifiedPPE pped) r
--     Just typ -> pure . P.group $
--       TypePrinter.prettySignatures
--         (PPE.suffixifiedPPE pped)
--         [(PPE.termName (PPE.unsuffixifiedPPE pped) r, typ)]
--   prettyEval terms r = case r of
--     Referent.Ref (Reference.Builtin n) -> pure . P.syntaxToColor $ P.text n
--     Referent.Ref ref ->
--       let ppe = PPE.declarationPPE pped ref
--       in  terms ref >>= \case
--             Nothing -> pure $ "😶  Missing term source for: " <> termName ppe r
--             Just tm -> pure $ TP.pretty ppe tm
--     Referent.Con r _ _ -> pure $ typeName (PPE.declarationPPE pped r) r
--   prettyTerm terms r = case r of
--     Referent.Ref (Reference.Builtin _) -> prettySignature r
--     Referent.Ref ref -> let ppe = PPE.declarationPPE pped ref in terms ref >>= \case
--       Nothing -> pure $ "😶  Missing term source for: " <> termName ppe r
--       Just tm -> pure . P.syntaxToColor $ P.group $ TP.prettyBinding ppe (PPE.termName ppe r) tm
--     Referent.Con r _ _ -> prettyType r
--   prettyType r = let ppe = PPE.declarationPPE pped r in types r >>= \case
--     Nothing -> pure $ "😶  Missing type source for: " <> typeName ppe r
--     Just ty -> pure . P.syntaxToColor $ P.group $ DP.prettyDecl ppe r (PPE.typeName ppe r) ty

termName ppe r =
  P.syntaxToColor $
    NP.styleHashQualified'' (NP.fmt $ S.Referent r) name
  where
    name = PPE.termName ppe r

typeName ppe r =
  P.syntaxToColor $
    NP.styleHashQualified'' (NP.fmt $ S.Reference r) name
  where
    name = PPE.typeName ppe r
