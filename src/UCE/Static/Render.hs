{-# LANGUAGE QuasiQuotes #-}
module UCE.Static.Render where

import UCE.Code
import UCE.Prelude
import qualified Data.Text
import qualified Unison.Referent as Referent
import qualified Data.Map.Strict as Map
import Unison.Reference (toShortHash)
import Unison.ShortHash (ShortHash(..))
import qualified Unison.HashQualified as Unison.HashQualified
import qualified Unison.Util.SyntaxText as SyntaxText
import Unison.Util.AnnotatedText ( AnnotatedText(..) )
import Data.String.QM
import UCE.Static.Organize (dots, itemHref)

renderPage :: [Text] -> Maybe Reference -> Map [Text] (Maybe Reference) -> p -> (ShortHash -> Maybe [Text]) -> Map [Text] Text -> CodeInfo -> Map [Text] (a, Map [Text] b) -> Text
renderPage path ref children _href hashRef hrefs codeinfo entryMap =
    htmlTop title
    [qt|<h1>${title}</h1>
    ${top}
    ${childrenHtml}
    |]
    where
        title = dots path
        top = case ref of
            Nothing -> ""
            Just ref' -> showItem hrefs hashRef ref' codeinfo
        childrenHtml = childrenListing path children hashRef hrefs codeinfo entryMap

divv :: Text -> Text
divv text = "<div>" <> text <> "</div>"

a :: Text -> Map [Text] Text -> [Text] -> Text -> Text
a cls hrefs path contents = case (Map.lookup path hrefs) of
    Nothing -> contents
    Just href -> [qt|<a class="${cls}" href="${href}">${contents}</a>|]

childrenListing :: p -> Map [Text] (Maybe Reference) -> (ShortHash -> Maybe [Text]) -> Map [Text] Text -> CodeInfo -> Map [Text] (a, Map [Text] b) -> Text
childrenListing _path children hashRef hrefs codeinfo entryMap =
    children & Map.toAscList & map makeChild & Data.Text.intercalate "\n"
    where
        makeChild (path, ref) = [qt|
        <div>
            <h1 id="${id}">${link}</h1>
            ${sub}
            ${body}
        </div>
        |]
             where
                 body = makeBody path ref
                 sub = subItems path
                 id = case ref of
                    Nothing -> ""
                    Just ref' -> itemHref ref'
                 link = a "" hrefs path (dots path)
        subItems path = case (Map.lookup path entryMap) of
            Nothing -> ""
            Just (_, children') -> if Map.size children' == 0
                then ""
                else
                    "<div class='children'>" <> (children' & Map.toAscList & map (divv . makeSubChild path) & Data.Text.intercalate "\n") <> "</div>"
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
        Nothing -> [qt|<span class="${cls}">${escaped}</span>|]
        Just h -> a cls hrefs h (escaped)
    where
        escaped = escapeHTML contents
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

htmlTop :: Text -> Text -> Text
htmlTop title body = [qt|<!doctype html>
<head>
    <meta charset=utf8>
    <title>${title}</title>
    <style>${style}</style>
</head>
<body>
    ${body}
</body>|]

style :: Text
style = [qt|
 body {
     max-width: 800px;
     margin: 0 auto;
     font-size: 16px;
     font-family: system-ui;
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
 .sub-name a {
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