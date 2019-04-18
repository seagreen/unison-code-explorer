module Lib where

import Control.Monad (when)
import Prelude
import Unison.Codebase (Codebase)
import Unison.Codebase.Serialization.V0 (formatSymbol)
import Unison.Parser (Ann(External))
import Unison.Symbol (Symbol)

import qualified Unison.Codebase.Editor as Editor
import qualified Unison.Codebase.FileCodebase as FileCodebase
import qualified Unison.Codebase.Serialization as S

data Config = Config Bool String deriving Show

run :: Config -> IO ()
run _ = do
  let
    codebasePath :: FilePath
    codebasePath =
      ".unison"

    theCodebase :: Codebase IO Symbol Ann
    theCodebase =
      FileCodebase.codebase1 External formatSymbol formatAnn codebasePath

  exists <- FileCodebase.exists codebasePath
  if exists
    then do
      Editor.initializeCodebase theCodebase
      putStrLn "Success"
    else
      putStrLn "No codebase found"

formatAnn :: S.Format Ann
formatAnn =
  S.Format (pure External) (\_ -> pure ())

one :: Int
one =
  1
