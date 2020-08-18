{-# Language PatternSynonyms #-}
{-# Language OverloadedStrings #-}

module UCE.Static.DisplayDoc where

-- import Data.Foldable ( fold )

import UCE.Prelude
-- import Unison.Reference (Reference)
-- import Unison.Referent (Referent)
-- import Unison.Term (Term)
-- import Unison.Type (Type)
-- import Unison.Var (Var)
import qualified Unison.Builtin.Decls as DD
import qualified Unison.DataDeclaration as DD
import qualified Unison.DeclPrinter as DP
import qualified Unison.NamePrinter as NP
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Referent as Referent
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.TermPrinter as TP
import qualified Unison.TypePrinter as TypePrinter
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.SyntaxText as S
import qualified Data.Text
import Data.Text (pack, unpack)

import UCE.Static.Utils

type Pretty = P.Pretty P.ColorText


-- displayTerm pped terms typeOf eval types tm = case tm of
--   Term.Ref' r -> eval r >>= \case
--     Nothing -> pure $ termName (PPE.suffixifiedPPE pped) (Referent.Ref r)
--     Just tm -> displayDoc pped terms typeOf eval types tm
--   _ -> displayDoc pped terms typeOf eval types tm

displayDoc showLink showItem showSignature showResult = go
  where
  go (DD.DocJoin docs) = docs & toList & map go & Data.Text.concat
  go (DD.DocBlob txt) = txt & unpack & escapeHTML
  go (DD.DocLink (DD.LinkTerm (Term.TermLink' r))) = showLink (Referent.toReference r)
  go (DD.DocLink (DD.LinkType (Term.TypeLink' r))) = showLink r
  go (DD.DocSource (DD.LinkTerm (Term.TermLink' r))) = showItem (Referent.toReference r)
  go (DD.DocSource (DD.LinkType (Term.TypeLink' r))) = showItem r
  go (DD.DocSignature (Term.TermLink' r)) = showSignature r
  go (DD.DocEvaluate (Term.TermLink' r)) = showResult r
  go tm = show tm & pack
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

termName ppe r = P.syntaxToColor $
  NP.styleHashQualified'' (NP.fmt $ S.Referent r) name
  where name = PPE.termName ppe r

typeName ppe r = P.syntaxToColor $
  NP.styleHashQualified'' (NP.fmt $ S.Reference r) name
  where name = PPE.typeName ppe r
