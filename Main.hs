{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Options.Applicative
import Paths_unison_code_explorer (version)
import UCE.Prelude

import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder
import qualified UCE
import qualified UCE.Load as Load

data Config = Config
  { configDumpJson :: Bool
  , configPort :: Int
  } deriving (Show)

main :: IO ()
main = do
  logLn "Starting"
  conf <- runParser
  api <- Load.load
  if configDumpJson conf
    then do
      TIO.putStrLn . encodePretty . object $
        [ "names" .= Load.apiNames api
        , "function_call_graph" .= Load.apiFcg api
        ]

    else
      UCE.run (configPort conf) api
 where
  runParser :: IO Config
  runParser =
    customExecParser (prefs showHelpOnError) parserInfo

  parserInfo :: ParserInfo Config
  parserInfo =
    info
      (helper <*> versionOption <*> parser)
      (  fullDesc
      <> progDesc "Make some details of a .unison codebase available over HTTP."
      )

  versionOption :: Parser (a -> a)
  versionOption =
    infoOption
      (showVersion version <> " " <> $(gitHash))
      (  long "version"
      <> help "Show version"
      )

  -- Make sure you include the `help` section or that flag won't show up
  -- under "Available options".
  parser :: Parser Config
  parser =
    Config
      <$>
        switch
          (  long "dump-json"
          <> help "Dump JSON to STDOUT instead of starting server"
          )
      <*>
        option auto
          (  long "port"
          <> help "Port to run server on"
          <> value 8080
          <> showDefault
          )

encodePretty :: ToJSON a => a -> Text
encodePretty =
    TL.toStrict
  . Data.Text.Lazy.Builder.toLazyText
  . Pretty.encodePrettyToTextBuilder' conf
  where
    conf :: Pretty.Config
    conf = Pretty.Config
      { Pretty.confIndent = Pretty.Spaces 2
      , Pretty.confCompare = compare
      , Pretty.confNumFormat = Pretty.Decimal
      , Pretty.confTrailingNewline = False
      }
