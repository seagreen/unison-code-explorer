{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Options.Applicative
import Paths_unison_code_explorer (version)
import UCE.Prelude

import qualified UCE
import qualified UCE.CodeInfo as CodeInfo

data Config = Config
  { configPort :: Int
  } deriving (Show)

main :: IO ()
main = do
  logLn "Starting"
  conf <- runParser
  codeinfo <- CodeInfo.load
  UCE.run (configPort conf) codeinfo
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
        option auto
          (  long "port"
          <> help "Port to run server on"
          <> value 8080
          <> showDefault
          )
