-- | Tweak the @Prelude@. Includes no domain logic.
module UCE.Prelude
  ( module UCE.Prelude
  , module X
  ) where

-- Re-exports:

import Prelude as X hiding (error, foldl, head, id, lookup)

import Control.Applicative as X
import Control.Concurrent as X
import Control.Concurrent.STM as X
import Control.Lens as X hiding ((.=), (<.>), children, from, index, set, to)
import Control.Monad as X
import Control.Monad.IO.Class as X
import Data.Either as X
import Data.Fixed as X
import Data.Foldable as X
import Data.IORef as X
import Data.Maybe as X
import Data.Text.Encoding as X hiding (decodeUtf8)
import Data.Time as X
import Data.Traversable as X
import Data.Void as X
import Debug.Trace as X
import System.Exit as X

import Data.ByteString as X (ByteString)
import Data.HashMap.Strict as X (HashMap)
import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Text as X (Text)
import GHC.Generics as X (Generic)
import Numeric.Natural as X (Natural)

-- Local stuff:

import GHC.Stack.Types (HasCallStack)

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Prelude
import qualified System.IO

data OneOf2 a b = One2 a | Two2 b

data OneOf3 a b c = One3 a | Two3 b | Three3 c

data OneOf4 a b c d = One4 a | Two4 b | Three4 c | Four4 d

data OneOf5 a b c d e = One5 a | Two5 b | Three5 c | Four5 d | Five5 e

setSwap :: Ord a => a -> Set a -> Set a
setSwap a set
  | Set.member a set = Set.delete a set
  | otherwise        = Set.insert a set

identity :: a -> a
identity a =
  a

{-# WARNING error "'error' remains in code" #-}
error :: HasCallStack => [Char] -> a
error =
  Prelude.error

panic :: HasCallStack => Text -> a
panic =
  error . Text.unpack

logLn :: [Char] -> IO ()
logLn =
  System.IO.hPutStrLn System.IO.stderr
