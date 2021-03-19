{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Huha.Build
import Data.Text.Internal as TI (Text)

data Options = Options
  { optInput :: String
  , optOutput :: String
  , optFile :: String
  , optHeader :: TI.Text
  , optSeparator :: TI.Text
  , optDryRun :: Bool }

input :: Parser String
input = strOption
  (  long "input"
  <> short 'i'
  <> showDefault
  <> value "."
  <> help "Input directory to build"
  <> metavar "PATH")

output :: Parser String
output = strOption
  (  long "output"
  <> short 'o'
  <> showDefault
  <> value "."
  <> help "Location to create a 'public' directory and build site to"
  <> metavar "PATH")

filename :: Parser String
filename = strOption
  (  long "file"
  <> short 'f'
  <> help "Filename of markdown file. It should be in the 'content' directory."
  <> metavar "FILE")

headerPattern :: Parser TI.Text
headerPattern = strOption
  (  long "header"
  <> short 'h'
  <> showDefault
  <> value "## 2"
  <> help "Header pattern to use in searching the begning of lines for headers." )

separator :: Parser TI.Text
separator = strOption
  (  long "separator"
  <> short 's'
  <> showDefault
  <> value "------"
  <> help "Separator pattern to use to split up the file." )

--verbose :: Parser Bool
--verbose = switch ( long "verbose" <> short 'v' <> help "Display build information" )

dryRun :: Parser Bool
dryRun = switch ( long "dry" <> help "Dry run the build. The output directory structure will still be created (if needed)." )

combined :: Parser Options
combined = Options <$> input <*> output <*> filename <*> headerPattern <*> separator <*> dryRun

opts :: ParserInfo Options
opts = info (combined <**> helper)
  ( fullDesc
  <> progDesc "Build a static site from a single file." -- TODO
  <> header "huha - static site generator")


main :: IO ()
main = do
  options <- execParser opts
  let config = createConfig
              (optInput options)
              (optFile options)
              (optOutput options)
              (optHeader options)
              (optSeparator options)

  prepareBuild config
  if optDryRun options
     then putStrLn "dry run - nothing built"
  else runBuild config
