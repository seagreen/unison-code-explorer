{-# LANGUAGE TemplateHaskell #-}

module Cli where

import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Options.Applicative
import Paths_unison_browser (version)
import Prelude

import qualified Lib

main :: IO ()
main =
  Lib.run =<< runParser
 where
  runParser :: IO Lib.Config
  runParser =
    customExecParser (prefs showHelpOnError) parserInfo

  parserInfo :: ParserInfo Lib.Config
  parserInfo =
    info
      (helper <*> versionOption <*> parser)
      (  fullDesc
      <> header "Example header"
      <> progDesc "Example program description."
      )

  versionOption :: Parser (a -> a)
  versionOption =
    infoOption
      (showVersion version <> " " <> $(gitHash))
      (  long "version"
      <> help "Show version"
      )

  -- Make sure you include the `help` section or that flag won't show up
  -- under "Available options". Instead you'll get:
  --
  --     Usage: game-server [--version] [-p|--port PORT]
  --       Example program description.
  --     Available options:
  --       -h,--help                Show this help text
  --       --version                Show version
  --
  parser :: Parser Lib.Config
  parser =
    Lib.Config
      <$> switch
          (  long "bool_param"
          <> short 'b'
          <> help "bool parameter"
          )
      <*> strOption
          (  long "string_param"
          <> short 's'
          <> metavar "STRING"
          <> help "string parameter"
          <> value "default_value"
          <> showDefault
          )
