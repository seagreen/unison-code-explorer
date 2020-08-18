-- | Tweak @Relude@. Includes no domain logic.
module UCE.Prelude
  ( module UCE.Prelude,
    module X,
  )
where

{- ORMOLU_DISABLE -}

-- Re-exports:

import Relude as X hiding (error, id)

import Data.Traversable as X (for)

-- Local stuff:

import qualified Data.Set as Set
import qualified Prelude
import qualified System.IO

{- ORMOLU_ENABLE -}

data OneOf2 a b = One2 a | Two2 b

data OneOf3 a b c = One3 a | Two3 b | Three3 c

data OneOf4 a b c d = One4 a | Two4 b | Three4 c | Four4 d

data OneOf5 a b c d e = One5 a | Two5 b | Three5 c | Four5 d | Five5 e

{-# WARNING error "'error' remains in code" #-}
error :: Text -> a
error =
  Prelude.error . toString

panic :: Text -> a
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
