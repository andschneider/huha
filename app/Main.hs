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
--    print html

  -- TODO merge into one function?
  fileLines <- getLines fileName
  let headers = Prelude.map parseHeader (filterLines fileLines pattern)
  let sortedUnique = getUniqueTags headers
  mapM (writeBlankNote dir) sortedUnique

  -- print headers
  -- print "---"
  print sortedUnique

  -- TODO kinda janky. better way to ignore until first pattern match?
  let removeFirstTwo = tail (tail fileLines)
  let test = extractUntil (\x -> not $ T.isPrefixOf "------" x) removeFirstTwo
  print test

  -- TODO combine these
  let yeet = createNotes removeFirstTwo "------"
  print yeet

  mapM (appendNote dir) yeet
  writeNotes dir sortedUnique html
