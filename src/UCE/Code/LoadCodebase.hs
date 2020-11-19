module UCE.Code.LoadCodebase
  ( loadCodebaseAndBranch,
  )
where

import Data.Map.Strict ()
import UCE.Prelude hiding (head)
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch0 (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.FileCodebase as FileCodebase
import qualified Unison.Codebase.Serialization as Serialization
import Unison.Codebase.Serialization.V1 (formatSymbol)
import Unison.Parser (Ann (External))
import Unison.Symbol (Symbol)

loadCodebaseAndBranch :: String -> IO (Codebase IO Symbol Ann, Branch0 IO)
loadCodebaseAndBranch projectDirectory = do
  cache <- Branch.boundedCache 4096
  let codebasePath :: FilePath
      codebasePath = projectDirectory
  codebase <- FileCodebase.codebase1 cache formatSymbol formatAnn codebasePath

  exists <- FileCodebase.codebaseExists codebasePath
  when (not exists) (die ("No codebase found in " <> codebasePath))

  branch' <- Codebase.getRootBranch codebase
  let branch_ :: IO (Branch.Branch IO)
      branch_ = case branch' of
        Left _ -> die "Unable to load root branch"
        Right branch -> pure branch
  branch <- branch_

  let head :: Branch0 IO
      head =
        Branch.head branch

  pure (codebase, head)
  where
    formatAnn :: Serialization.Format Ann
    formatAnn =
      Serialization.Format (pure External) (\_ -> pure ())
