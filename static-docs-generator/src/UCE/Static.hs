{-# LANGUAGE QuasiQuotes #-}

module UCE.Static where

import qualified Data.Map.Strict as Map
import qualified Data.Text
import qualified System.Directory
import UCE.Code
import UCE.Prelude
import qualified UCE.Static.Organize as Organize
import qualified UCE.Static.Render as Render
import Unison.ShortHash (ShortHash (..))

buildStatic :: String -> String -> IO ()
buildStatic projectDirectory outputDirectory = do
  codeinfo <- UCE.Code.load projectDirectory
  UCE.Static.build outputDirectory codeinfo

build :: String -> UCE.Code.CodeInfo -> IO ()
build dest codeinfo = do
  _ <- System.Directory.createDirectoryIfMissing True dest
  let (paths, scopeMap) = Organize.makeScopeMap codeinfo
  let hashMap = Organize.makeHashMap paths
  let hrefMap = Organize.makeHrefMap scopeMap
  let hashRef hash = Map.lookup hash hashMap
  _ <- Map.toList scopeMap & mapM (writePage dest hashRef hrefMap codeinfo scopeMap)
  pure ()

writePage :: String -> (ShortHash -> Maybe [Text]) -> Map [Text] Text -> CodeInfo -> Map [Text] (a, Map [Text] b) -> ([Text], (Maybe Reference, Map [Text] (Maybe Reference))) -> IO ()
writePage dest hashRef hrefs codeinfo entryMap (path, (ref, children)) =
  if Map.size children == 0
    then pure ()
    else case (Map.lookup path hrefs) of
      Nothing -> pure ()
      Just href -> writeFile (dest <> "/" <> Data.Text.unpack href) (Render.renderPage path ref children href hashRef hrefs codeinfo entryMap & Data.Text.unpack)
