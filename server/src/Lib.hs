module Lib where

import Control.Monad (when)
import Prelude
import Safe (headMay)
import System.Environment (getArgs)
import Unison.Codebase.Serialization.V0 (formatSymbol)
import Unison.Parser (Ann(External))

import qualified Unison.Codebase.Editor as Editor
import qualified Unison.Codebase.FileCodebase as FileCodebase
import qualified Unison.Codebase.Serialization as S
import qualified Unison.CommandLine.Main as CommandLine
import qualified Unison.Runtime.Rt1IO as Rt1

data Config = Config Bool String deriving Show

run :: Config -> IO ()
run _ = do
  args <- getArgs
  -- hSetBuffering stdout NoBuffering -- cool
  let codebasePath  = ".unison"
      initialBranchName = "master"
      scratchFilePath   = "."
      theCodebase =
        FileCodebase.codebase1 External formatSymbol formatAnn codebasePath
      launch = CommandLine.main
        scratchFilePath
        initialBranchName
        (headMay args)
        (pure Rt1.runtime)
        theCodebase
  exists <- FileCodebase.exists codebasePath
  if exists
    then do
      Editor.initializeCodebase theCodebase
      launch
    else
      putStrLn "No codebase found"

formatAnn :: S.Format Ann
formatAnn = S.Format (pure External) (\_ -> pure ())

one :: Int
one =
  1
