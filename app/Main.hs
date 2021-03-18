{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Notes
import Parser
import System.FilePath (joinPath)

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
  mapM_ (writeBlankNote dir) sortedUnique

  -- print headers
  -- print "---"
  -- print sortedUnique

  -- TODO kinda janky. better way to ignore until first pattern match?
  let removeFirstTwo = tail (tail fileLines)
  let test = extractUntil (\x -> not $ T.isPrefixOf "------" x) removeFirstTwo
  -- print test

  -- TODO combine these
  let yeet = createNotes removeFirstTwo "------"
  -- print yeet

  -- TODO print out which files are being written
  mapM_ (appendNote dir) yeet
  writeNotes dir sortedUnique html
