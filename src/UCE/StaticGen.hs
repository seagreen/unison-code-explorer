module UCE.StaticGen where

import UCE.Code
import System.Directory (createDirectory)
import UCE.Prelude
import qualified Data.Text
import UCE.DeclarationJson
import qualified Unison.Util.Relation as Relation
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Data.Map.Strict as Map
import Unison.Reference (toShortHash)
import qualified Unison.Hash as Hash
import Unison.ShortHash (ShortHash(..))
import qualified Unison.HashQualified as Unison.HashQualified
import qualified Unison.Util.SyntaxText as SyntaxText
import Unison.Util.AnnotatedText ( AnnotatedText(..) )

type ScopeMap = Map [Text] (Maybe Reference, [([Text], Reference)])

dots :: [Text] -> Text
dots = Data.Text.intercalate "."

parentPath :: [a] -> [a]
parentPath [] = []
parentPath path = reverse path & tail & reverse

indexHref :: [Text] -> Text
indexHref [] = "index"
indexHref path = dots path

itemHref :: Reference -> Text
itemHref (Reference.Builtin b) = "__builtin__" <> b
itemHref (Reference.Derived h _ 1) = (Hash.base32Hex h)
itemHref (Reference.Derived h i n) = (Hash.base32Hex h) <> Reference.showSuffix i n

makeHref :: [Text] -> Maybe Reference -> ChildMap -> EntryMap -> Text
makeHref path Nothing _ _ = indexHref path <> ".html"
makeHref path (Just child) children scopeMap = 
    if Map.size children == 0 then
        (case (Map.lookup (parentPath path) scopeMap) of
        Nothing -> "FAILURE" & Data.Text.pack
        Just (Nothing, _) -> indexHref (parentPath path)
        Just (Just r, _) -> itemHref r) <> ".html#" <> itemHref child
    else
        itemHref child <> ".html"

makeHrefMap :: EntryMap -> Map [Text] Text
makeHrefMap scopeMap =
    Map.mapWithKey f scopeMap
    where
        f path (ref, children) = makeHref path ref children scopeMap


type ChildMap = Map [Text] (Maybe Reference)
type EntryMap = Map [Text] (Maybe Reference, ChildMap)

makeScopeMap :: CodeInfo -> ([([Text], Reference)], Map [Text] (Maybe Reference, ChildMap))
makeScopeMap codeinfo =
    let names = codeinfo & UCE.Code.codeDeclarationNames & Relation.domain & Map.toAscList
        refs = map fst names
        paths = map (\r -> (plainName codeinfo r & fst & Data.Text.splitOn ".", r)) refs
    in
        (paths, foldl f (Map.fromList []) paths)
    where
        addToChildren :: ChildMap -> [Text] -> (Maybe Reference) -> ChildMap
        addToChildren children path Nothing = case (Map.lookup path children) of
            Nothing -> Map.insert path Nothing children
            _ -> children
        addToChildren children path (Just ref) = Map.insert path (Just ref) children

        addEntry entryMap path ref = 
            let entryMap' = Map.insert path (ref, Map.empty) entryMap
            in
                addToParent entryMap' path ref

        addToParent mmap path r =
            case (Map.lookup parent mmap) of
                Nothing -> addEntry mmap parent Nothing
                Just (item, children) -> Map.insert parent (item, addToChildren children path r) mmap
            where
                parent = parentPath path
        f mmap (path, ref) = addEntry mmap path (Just ref)

-- Make a mapping which will allow us to create pages
makeScopeMap' :: UCE.Code.CodeInfo -> ScopeMap
makeScopeMap' codeinfo =
    let names = codeinfo & UCE.Code.codeDeclarationNames & Relation.domain & Map.toAscList
        refs = map fst names
        refsAndNames = map (\r -> ((plainName codeinfo r & fst & Data.Text.splitOn ".", r))) refs
        asMap :: ScopeMap
        asMap = map
            (\(text, ref) -> (text, (Just ref, childRefs text refsAndNames)))
            refsAndNames & Map.fromList
        withIndices = foldl (\mmap (name, _ref) -> foldl (buildUpIndices refsAndNames) mmap (subNames name)) asMap refsAndNames
    in
        withIndices
    where
        subNames :: [Text] -> [[Text]]
        subNames name =
            foldl (\(path, res) current -> (concat [path, [current]], path : res)) ([], []) name & snd
        buildUpIndices :: [([Text], Reference)] -> ScopeMap -> [Text] -> ScopeMap
        buildUpIndices refsAndNames mmap name =
            case (Map.lookup name mmap) of
                Nothing -> Map.insert name (Nothing, childRefs name refsAndNames) mmap
                Just _ -> mmap

makeHashMap :: Foldable t => t (a, Reference) -> Map ShortHash a
makeHashMap paths = foldl f Map.empty paths
    where
        f mmap (path, r) = Map.insert (Reference.toShortHash r) path mmap

build :: String -> UCE.Code.CodeInfo -> IO ()
build dest codeinfo = do
    -- _ <- createDirectory dest
    let (paths, scopeMap) = makeScopeMap codeinfo
    let hashMap = makeHashMap paths
    let hrefMap = makeHrefMap scopeMap
    let hashRef hash = Map.lookup hash hashMap
    _ <- Map.toList scopeMap & mapM (writePage dest hashRef hrefMap codeinfo scopeMap)
    pure ()

htmlTop :: Text -> Text
htmlTop title = "<!doctype html><head><title>" <> title <> "</title><style>" <> style <> "</style></head><body>\n"
htmlBottom :: Text
htmlBottom = "\n</body></html>"

writePage :: String -> (ShortHash -> Maybe [Text]) -> Map [Text] Text -> CodeInfo -> Map [Text] (a, Map [Text] b) -> ([Text], (Maybe Reference, Map [Text] (Maybe Reference))) -> IO ()
writePage dest hashRef hrefs codeinfo entryMap (path, (ref, children)) =
    if Map.size children == 0 then
        pure ()
    else
        case (Map.lookup path hrefs) of
            Nothing -> pure ()
            Just href -> writeFile (dest <> "/" <> Data.Text.unpack href) (renderPage path ref children href hashRef hrefs codeinfo entryMap & Data.Text.unpack)

isChild :: Eq a => [a] -> [a] -> Bool
isChild base ref = base == parentPath ref

childRefs :: Eq a => [a] -> [([a], b)] -> [([a], b)]
childRefs base refs =
    filter (isChild base . fst) refs

renderPage :: [Text] -> Maybe Reference -> Map [Text] (Maybe Reference) -> p -> (ShortHash -> Maybe [Text]) -> Map [Text] Text -> CodeInfo -> Map [Text] (a, Map [Text] b) -> Text
renderPage path ref children _href hashRef hrefs codeinfo entryMap =
    [htmlTop (dots path)
    , "<h1>" <> (dots path) <> "</h1>"
    , case ref of
        Nothing -> ""
        Just ref' -> showItem hrefs hashRef ref' codeinfo
    , childrenListing path children hashRef hrefs codeinfo entryMap
    , htmlBottom] & Data.Text.intercalate "\n"

divv :: Text -> Text
divv text = "<div>" <> text <> "</div>"

a :: Text -> Map [Text] Text -> [Text] -> Text -> Text
a cls hrefs path contents = case (Map.lookup path hrefs) of
    Nothing -> contents
    Just href -> "<a class=\"" <> cls <> "\" href=\"" <> href <> "\">" <> contents <> "</a>"

childrenListing :: p -> Map [Text] (Maybe Reference) -> (ShortHash -> Maybe [Text]) -> Map [Text] Text -> CodeInfo -> Map [Text] (a, Map [Text] b) -> Text
childrenListing _path children hashRef hrefs codeinfo entryMap =
    children & Map.toAscList & map (divv . makeChild ) & Data.Text.intercalate "\n"
    where
        makeChild (path, ref) = "<h1 id=\"" <> (case ref of
             Nothing -> ""
             Just ref' -> itemHref ref')
             <> "\">"
             <> a "" hrefs path (dots path) <> "</h1>"
             <> makeBody path ref <> subItems  path
        subItems path = case (Map.lookup path entryMap) of
            Nothing -> ""
            Just (_, children') -> if Map.size children' == 0 then "" else
                "<div class='children'>" <> (children' & Map.toAscList & map (divv . makeSubChild path) & Data.Text.intercalate "\n") <> "</div>"
        makeSubChild parentPath' (childPath, _) = a "sub-name" hrefs childPath (dots (drop (length parentPath') childPath))
        makeBody _path Nothing = ""
        makeBody _path (Just ref) = divv (showItem hrefs hashRef ref codeinfo)

showItem :: Map [Text] Text -> (ShortHash -> Maybe [Text]) -> Reference -> CodeInfo -> Text
showItem hrefs hashRef ref codeinfo =
    "<code><pre>" <> body <> "</code></pre>"
    where
        body = case Map.lookup ref (codeBodies codeinfo) of
            Nothing -> "No BODY FOUND"
            Just t -> codeBody hrefs hashRef t

codeBody :: Map [Text] Text -> (ShortHash -> Maybe [Text]) -> AnnotatedText SyntaxText.Element -> Text
codeBody hrefs hashRef (AnnotatedText items) =
    toList items & map (renderElement hrefs hashRef) & Data.Text.concat

escapeHTML :: String -> Text
escapeHTML text = text & Data.Text.pack
    & Data.Text.replace "&" "&amp;"
    & Data.Text.replace "<" "&lt;"
    & Data.Text.replace ">" "&gt;"

refHash :: ShortHash -> (Text, Bool)
refHash (Builtin b) = (b, False)
refHash (ShortHash h Nothing _) = (h, True)
refHash (ShortHash h (Just suffix) _) = (h <> suffix, True)


renderElement :: Map [Text] Text -> (ShortHash -> Maybe [Text]) -> (String, Maybe SyntaxText.Element) -> Text
renderElement _hrefs _hashRef (contents, Nothing) = escapeHTML contents
renderElement hrefs hashRef (contents, Just kind) = 
    case (hash >>= hashRef) of
        Nothing -> "<span class=\"" <> cls <> "\">" <> escapeHTML contents <> "</span>"
        Just h -> a cls hrefs h (escapeHTML contents)
    where
        hash = case kind of
                SyntaxText.Reference r -> Just $ Unison.Reference.toShortHash r
                SyntaxText.Referent  r -> Just $ Unison.Reference.toShortHash (Referent.toReference r)
                SyntaxText.HashQualifier hq -> case (Unison.HashQualified.toHash hq) of
                    Nothing -> Nothing
                    Just hash' -> Just hash'
                _ -> Nothing
        cls = case kind of
            SyntaxText.Reference _ -> "Reference"
            SyntaxText.Referent _ -> "Referent"
            SyntaxText.HashQualifier _ -> "HashQualifier"
            _ -> show kind & Data.Text.pack



style :: Text
style = "\
\ body {\
\     max-width: 800px;\
\     margin: 0 auto;\
\     font-size: 16px;\
\     font-family: system-ui;\
\ }\
\ \ 
\ pre {\
\     padding: 10px 20px;\
\     box-shadow: 0 0 2px;\
\     border-radius: 3px;\
\ }\
\  \
\ .sub-name {\
\     display: inline-block;\
\     padding: 4px 8px;\
\     margin-right: 8px;\
\     margin-bottom: 8px;\
\     border-radius: 5px;\
\     background-color: #ccc;\
\     font-size: 80%;\
\ }\
\ .children {\
\   display: flex; flex-direction: row; flex-wrap: wrap;\
\ }\
\  \
\ .Constructor {\
\     color: #ce8500\
\ }\
\ .DataTypeKeyword {\
\     font-weight: bold;\
\ }\
\ .Var {\
\     color: #00a;\
\ }\
\ .NumericLiteral, .TextLiteral, .CharLiteral, .BooleanLiteral {\
\     color: green;\
\ }\
\ \ 
\   .ControlKeyword      { font-weight: bold }\
\   .AbilityBraces       ,\
\   .LinkKeyword         ,\
\   .TypeOperator        ,\
\   .UseKeyword          ,\
\   .UsePrefix           ,\
\   .UseSuffix           ,\
\   .HashQualifier      { color: #777 }\
\   .DelayForceChar      { color: yellow }\
\   .TypeAscriptionColon { color: blue }\
\   .DocDelimiter        { color: green }\
\   .DocKeyword          { font-weight: bold }\
\ "