{-# LANGUAGE PatternSynonyms #-}

module UCE.Static.DisplayDoc where

import UCE.Prelude
import qualified Unison.Builtin.Decls as DD
import Unison.Reference (Reference (..))
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Util.SyntaxText (SyntaxText)
import qualified Unison.Util.SyntaxText as S

data Element
  = Text Text
  | TermLink Referent.Referent
  | TypeLink Reference.Reference
  | TermSource S.SyntaxText
  | TypeSource S.SyntaxText
  | Eval S.SyntaxText
  | Signature S.SyntaxText
  deriving (Show)

displayDoc ::
  (Monad f, Show v) =>
  (Reference -> f SyntaxText) ->
  (Referent -> f SyntaxText) ->
  (Referent -> f SyntaxText) ->
  (Referent -> f SyntaxText) ->
  (Reference -> f (Maybe (Term v a))) ->
  Term v a ->
  f [Element]
displayDoc showTypeSource showTermSource showSignature showResult getInclude = go
  where
    go (DD.DocJoin docs) = fold <$> traverse go docs
    -- go (DD.DocJoin docs) = docs & toList & map go & List.concat
    go (DD.DocBlob txt) = pure [txt & Text]
    go (DD.DocLink (DD.LinkTerm (Term.TermLink' r))) =
      pure [TermLink r]
    go (DD.DocLink (DD.LinkType (Term.TypeLink' r))) =
      pure [TypeLink r]
    go (DD.DocSource (DD.LinkTerm (Term.TermLink' r))) = do
      source <- showTermSource r
      pure [TermSource source]
    go (DD.DocSource (DD.LinkType (Term.TypeLink' r))) = do
      source <- showTypeSource r
      pure [TypeSource source]
    go (DD.DocSignature (Term.TermLink' r)) = do
      sig <- showSignature r
      pure [Signature sig]
    go (DD.DocEvaluate (Term.TermLink' r)) = do
      res <- showResult r
      pure [Eval res]
    go (Term.Ref' r) = do
      evaled <- getInclude r
      case evaled of
        Nothing -> pure [Text "Unable to process include"]
        Just included -> go included
    go tm = pure [Text (show tm)]
