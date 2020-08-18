{-# LANGUAGE QuasiQuotes #-}
module UCE.Static where

import UCE.Code
import qualified System.Directory -- (createDirectory)
import UCE.Prelude
import qualified Data.Text
import qualified Data.Map.Strict as Map
import Unison.ShortHash (ShortHash(..))
import qualified UCE.Static.Organize as Organize
import qualified UCE.Static.Render as Render

build :: String -> UCE.Code.CodeInfo -> IO ()
build dest codeinfo = do
    _ <- System.Directory.createDirectoryIfMissing True dest
    let (paths, scopeMap) = Organize.makeScopeMap codeinfo
    let hashMap = Organize.makeHashMap paths
    let hrefMap = Organize.makeHrefMap scopeMap
    let hashRef hash = Map.lookup hash hashMap
    let debugTxt = Map.toList scopeMap
            & map (\(path, (idx, children)) ->
                    Data.Text.intercalate "." path
                    <> " "
                    <> (
                    case idx of
                        Nothing -> " [index] "
                        Just r -> " [ref] " <> (case Map.lookup r (codeBodies codeinfo) of
                            Nothing -> "NO BODY "
                            Just _ -> "A body ")
                    )
                    <> (Map.keys children & map (Data.Text.intercalate ".") & Data.Text.intercalate ", ")
                    )
            & Data.Text.intercalate "\n"
    _ <- writeFile (dest <> "/" <> "debug.txt") (Data.Text.unpack debugTxt)
    _ <- Map.toList scopeMap & mapM (writePage dest hashRef hrefMap codeinfo scopeMap)
    pure ()

writePage :: String -> (ShortHash -> Maybe [Text]) -> Map [Text] Text -> CodeInfo -> Map [Text] (a, Map [Text] b) -> ([Text], (Maybe Reference, Map [Text] (Maybe Reference))) -> IO ()
writePage dest hashRef hrefs codeinfo entryMap (path, (ref, children)) =
    if Map.size children == 0 then
        pure ()
    else
        case (Map.lookup path hrefs) of
            Nothing -> pure ()
            Just href -> writeFile (dest <> "/" <> Data.Text.unpack href) (Render.renderPage path ref children href hashRef hrefs codeinfo entryMap & Data.Text.unpack)



