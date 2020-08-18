{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Options.Applicative
import Paths_unison_code_explorer (version)
import qualified UCE
import UCE.Prelude

data Config = Config
  { configPort :: Int,
    directory :: String,
    json :: Bool,
    static :: Bool,
    dest :: String
  }
  deriving (Show)

main :: IO ()
main = do
  conf <- runParser
  if (json conf)
    then UCE.dumpJson (directory conf)
    else
      if (static conf)
        then UCE.buildStatic (directory conf) (dest conf)
        else do
          logLine ("Starting on port " <> show (configPort conf))
          UCE.run (configPort conf) (directory conf)
  where
    runParser :: IO Config
    runParser =
      customExecParser (prefs showHelpOnError) parserInfo
    parserInfo :: ParserInfo Config
    parserInfo =
      info
        (helper <*> versionOption <*> parser)
        ( fullDesc
            <> progDesc "Web UI for viewing a Unison codebase"
        )
    versionOption :: Parser (a -> a)
    versionOption =
      infoOption
        (showVersion version <> " " <> $(gitHash))
        ( long "version"
            <> help "Show version"
        )
    -- Make sure you include the `help` section or that flag won't show up
    -- under "Available options".
    parser :: Parser Config
    parser =
      Config
        <$> option
          auto
          ( long "port"
              <> help "Port to run server on"
              <> value 8080
              <> showDefault
          )
        <*> strOption
          ( long "directory"
              <> help "Project directory to explore"
              <> value "."
              <> showDefault
          )
        <*> switch
          ( long "json"
              <> help "Output a JSON dump of the project instead of running a server"
              <> showDefault
          )
        <*> switch
          ( long "static"
              <> help "Generate a static site instead of running a server"
              <> showDefault
          )
        <*> strOption
          ( long "dest"
              <> help "For --static, the destination to write the generated documentation"
              <> value "docs"
              <> showDefault
          )
