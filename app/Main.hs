{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.List (nub, sort)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import Notes
import Parser
import System.FilePath (joinPath)
import Text.Megaparsec
import Text.Mustache

main :: IO ()
main = do
  --  putStrLn "Enter the directory name: "
  --  dir <- getLine
  let dir = "./example"
  let content = joinPath [dir, "content/"]

  let fileName = joinPath [content, "notes-cs.md"]
  let pattern = "##"
  --  let pattern = "------"

  raw <- getFile fileName
  let html = convertFile raw
  --  print html

  -- TODO merge into one function?
  ls <- getLines fileName
  let headers = checkLine ls pattern
  let tags = Prelude.map parseHeader headers -- TODO parse full header
  let parsedTags = Prelude.map parseTags tags
  let sortedUnique = sort (nub (Prelude.concat parsedTags))

  --  printLines headers
  --  print tags
  --  printLines tags
  --  mapM_ printLines parsedTags

  writeNotes dir sortedUnique html
