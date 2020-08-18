{-# LANGUAGE QuasiQuotes #-}

module UCE.Static.Utils where

import qualified Data.Map.Strict as Map
import Data.String.QM
import qualified Data.Text
import UCE.Prelude

escapeHTML :: String -> Text
escapeHTML text =
  text & Data.Text.pack
    & Data.Text.replace "&" "&amp;"
    & Data.Text.replace "<" "&lt;"
    & Data.Text.replace ">" "&gt;"

a :: Text -> Map [Text] Text -> [Text] -> Text -> Text
a cls hrefs path contents = case (Map.lookup path hrefs) of
  Nothing -> contents
  Just href -> [qt|<a class="${cls}" href="${href}">${contents}</a>|]
