{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Options.Applicative
import Paths_unison_static_docs (version)
import UCE.Prelude
import qualified UCE.Static

data Config = Config
  { directory :: String,
    dest :: String
  }
  deriving (Show)

main :: IO ()
main = do
  conf <- runParser
  UCE.Static.buildStatic (directory conf) (dest conf)
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
        <$> strOption
          ( long "directory"
              <> help "Project directory to explore"
              <> value "."
              <> showDefault
          )
        <*> strOption
          ( long "dest"
              <> help "For --static, the destination to write the generated documentation"
              <> value "docs"
              <> showDefault
          )
