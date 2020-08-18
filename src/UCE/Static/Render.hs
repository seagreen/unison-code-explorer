{-# LANGUAGE QuasiQuotes #-}

module UCE.Static.Render where

import Data.Foldable as X (foldl)
import qualified Data.List
import qualified Data.Map.Strict as Map
import Data.String.QM
import qualified Data.Text
import UCE.Code
import UCE.Prelude
import UCE.Static.Organize (dots, itemHref, parentPath)
import UCE.Static.Utils
import qualified Unison.HashQualified as Unison.HashQualified
import Unison.Reference (toShortHash)
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash (..))
import Unison.Util.AnnotatedText (AnnotatedText (..))
import qualified Unison.Util.SyntaxText as SyntaxText

renderBreadcrumb :: [Text] -> Map [Text] Text -> Text
renderBreadcrumb path hrefs =
  a "item" hrefs [] "Home" <> " : "
    <> (foldl process ([], []) path & snd & Data.Text.intercalate " . ")
  where
    process :: ([Text], [Text]) -> Text -> ([Text], [Text])
    process (path', result) current =
      (full, Data.List.concat [result, [a "item" hrefs full current]])
      where
        full = Data.List.concat [path', [current]]

renderPage :: [Text] -> Maybe Reference -> Map [Text] (Maybe Reference) -> p -> (ShortHash -> Maybe [Text]) -> Map [Text] Text -> CodeInfo -> Map [Text] (a, Map [Text] b) -> Text
renderPage path ref children _href hashRef hrefs codeinfo entryMap =
  htmlTop
    title
    [qt|
    ${breadcrumb}
    <h1>${title}</h1>
    ${top}
    ${childrenHtml}
    |]
  where
    breadcrumb = if path == [] then "" else "<div class='breadcrumb'>" <> renderBreadcrumb (parentPath path) hrefs <> "</div>"
    title = dots path
    top = case ref of
      Nothing -> ""
      Just ref' -> showItem hrefs hashRef ref' codeinfo
    childrenHtml = childrenListing path children hashRef hrefs codeinfo entryMap

divv :: Text -> Text
divv text = "<div>" <> text <> "</div>"

childrenListing :: [Text] -> Map [Text] (Maybe Reference) -> (ShortHash -> Maybe [Text]) -> Map [Text] Text -> CodeInfo -> Map [Text] (a, Map [Text] b) -> Text
childrenListing _path children hashRef hrefs codeinfo entryMap =
  children & Map.toAscList & map makeChild & Data.Text.intercalate "\n"
  where
    makeChild (path, ref) =
      [qt|
            <div>
                <h2 id="${id}">${link}</h2>
                ${sub}
                ${body}
            </div>
            |]
      where
        --  debugPath = Data.Text.intercalate ", " (map (\f -> "\"" <> f <> "\"") path) <> (
        --      if ref == Nothing
        --          then " [index] "
        --          else " [item] "
        --     ) <> (Map.size subChildren & show & Data.Text.pack) <> " children"
        body = makeBody path ref
        subChildren = case (Map.lookup path entryMap) of
          Nothing -> Map.empty
          Just (_, children') -> children'
        sub = subItems path subChildren
        id = case ref of
          Nothing -> ""
          Just ref' -> itemHref ref'
        hasChildren = Map.size subChildren == 0
        shortName = (drop (length _path) path)
        link =
          if hasChildren
            then (dots shortName)
            else a "" hrefs path (dots shortName)

    subItems path subChildren =
      if Map.size subChildren == 0
        then ""
        else "<div class='children'>" <> (subChildren & Map.toAscList & map (divv . makeSubChild path) & Data.Text.intercalate "\n") <> "</div>"
    makeSubChild parentPath' (childPath, _) = a "sub-name" hrefs childPath (dots (drop (length parentPath') childPath))
    makeBody _path Nothing = ""
    makeBody _path (Just ref) = divv (showItem hrefs hashRef ref codeinfo)

showItem :: Map [Text] Text -> (ShortHash -> Maybe [Text]) -> Reference -> CodeInfo -> Text
showItem hrefs hashRef ref codeinfo =
  [qt|<code><pre>${body}</code></pre>|]
  where
    body = case Map.lookup ref (codeBodies codeinfo) of
      Nothing -> "No BODY FOUND"
      Just t -> codeBody hrefs hashRef t

codeBody :: Map [Text] Text -> (ShortHash -> Maybe [Text]) -> AnnotatedText SyntaxText.Element -> Text
codeBody hrefs hashRef (AnnotatedText items) =
  toList items & map (renderElement hrefs hashRef) & Data.Text.concat

refHash :: ShortHash -> (Text, Bool)
refHash (Builtin b) = (b, False)
refHash (ShortHash h Nothing _) = (h, True)
refHash (ShortHash h (Just suffix) _) = (h <> suffix, True)

renderElement :: Map [Text] Text -> (ShortHash -> Maybe [Text]) -> (String, Maybe SyntaxText.Element) -> Text
renderElement _hrefs _hashRef (contents, Nothing) = escapeHTML contents
renderElement hrefs hashRef (contents, Just kind) =
  case (hash >>= hashRef) of
    Nothing -> [qt|<span class="${cls}">${escaped}</span>|]
    Just h -> a cls hrefs h (escaped)
  where
    escaped = escapeHTML contents
    hash = case kind of
      SyntaxText.Reference r -> Just $ Unison.Reference.toShortHash r
      SyntaxText.Referent r -> Just $ Unison.Reference.toShortHash (Referent.toReference r)
      SyntaxText.HashQualifier hq -> case (Unison.HashQualified.toHash hq) of
        Nothing -> Nothing
        Just hash' -> Just hash'
      _ -> Nothing
    cls = case kind of
      SyntaxText.Reference _ -> "Reference"
      SyntaxText.Referent _ -> "Referent"
      SyntaxText.HashQualifier _ -> "HashQualifier"
      _ -> show kind & Data.Text.pack

htmlTop :: Text -> Text -> Text
htmlTop title body =
  [qt|<!doctype html>
<head>
    <meta charset=utf8>
    <title>${title}</title>
    <style>${style}</style>
</head>
<body>
    ${body}
</body>|]

style :: Text
style =
  [qt|
 body {
     max-width: 800px;
     margin: 0 auto;
     font-size: 16px;
     font-family: system-ui;
 }

 .breadcrumb {
     font-size: 80%;
     padding: 16px;
 }

 .breadcrumb .item {
 }
  
 pre {
     padding: 10px 20px;
     box-shadow: 0 0 2px;
     border-radius: 3px;
 }
  
 .sub-name {
     display: inline-block;
     padding: 4px 8px;
     margin-right: 8px;
     margin-bottom: 8px;
     border-radius: 5px;
     background-color: #ccc;
     font-size: 80%;
 }
 a.sub-name {
     text-decoration: none;
 }
 .children {
   display: flex; flex-direction: row; flex-wrap: wrap;
 }
  
 .Constructor {
     color: #ce8500
 }
 .DataTypeKeyword {
     font-weight: bold;
 }
 .Var {
     color: #00a;
 }
 .NumericLiteral, .TextLiteral, .CharLiteral, .BooleanLiteral {
     color: green;
 }
  
   .ControlKeyword      { font-weight: bold }
   .AbilityBraces       ,
   .LinkKeyword         ,
   .TypeOperator        ,
   .UseKeyword          ,
   .UsePrefix           ,
   .UseSuffix           ,
   .HashQualifier      { color: #777 }
   .DelayForceChar      { color: #adad00; }
   .TypeAscriptionColon { color: blue }
   .DocDelimiter        { color: green }
   .DocKeyword          { font-weight: bold }
|]
