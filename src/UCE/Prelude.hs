-- | Tweak @Relude@. Includes no app-specific logic.
module UCE.Prelude
  ( module UCE.Prelude,
    module X,
  )
where

{- ORMOLU_DISABLE -}

-- Re-exports
--
-- Both for Relude and bringing part of Unison into scope.

import Relude as X hiding (error, id)

import Data.Traversable as X (for)
import Unison.Codebase as X (Codebase)
import Unison.Codebase.Branch as X (Branch0)
import Unison.Codebase.Runtime as X (Runtime)
import Unison.HashQualified as X (HashQualified')
import Unison.Reference as X (Reference)
import Unison.Referent as X (Referent)
import Unison.Symbol as X (Symbol)
import Unison.Term as X (Term)
import Unison.Util.AnnotatedText as X (AnnotatedText(AnnotatedText))
import Unison.Util.Pretty as X (Pretty)
import Unison.Util.Relation as X (Relation)
import Unison.Util.SyntaxText as X (SyntaxText)

{- ORMOLU_ENABLE -}

-- Local imports:

import qualified Data.Set as Set
import qualified System.IO
import qualified Prelude

data OneOf2 a b = One2 a | Two2 b

data OneOf3 a b c = One3 a | Two3 b | Three3 c

data OneOf4 a b c d = One4 a | Two4 b | Three4 c | Four4 d

data OneOf5 a b c d e = One5 a | Two5 b | Three5 c | Four5 d | Five5 e

{-# WARNING error "'error' remains in code" #-}
error :: HasCallStack => Text -> a
error =
  Prelude.error . toString

panic :: HasCallStack => Text -> a
panic =
  Prelude.error . toString

logLine :: [Char] -> IO ()
logLine =
  System.IO.hPutStrLn System.IO.stderr

-- * List stuff

headMaybe :: [a] -> Maybe a
headMaybe = \case
  [] ->
    Nothing
  a : _ ->
    Just a

-- * Set stuff

setSwap :: Ord a => a -> Set a -> Set a
setSwap a set
  | Set.member a set = Set.delete a set
  | otherwise = Set.insert a set

setToMaybe :: Set a -> Maybe a
setToMaybe =
  headMaybe . Set.toAscList
