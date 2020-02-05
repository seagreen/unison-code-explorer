{-# LANGUAGE TemplateHaskell #-}

module Cli where

import Data.Aeson
import Data.Text (Text)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Options.Applicative
import Paths_unison_code_explorer (version)
import Prelude

import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder
import qualified Load
import qualified Serve

main :: IO ()
main = do
  conf <- runParser
  if Serve.configDumpJson conf
    then do
      api <- Load.load
      TIO.putStrLn . encodePretty . object $
        [ "names" .= Load.apiNames api
        , "function_call_graph" .= Load.apiFcg api
        ]

    else
      Serve.run conf
 where
  runParser :: IO Serve.Config
  runParser =
    customExecParser (prefs showHelpOnError) parserInfo

  parserInfo :: ParserInfo Serve.Config
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
  parser :: Parser Serve.Config
  parser =
    Serve.Config
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
