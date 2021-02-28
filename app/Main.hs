{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.List (nub, sort)
import Data.Text
import qualified Data.Text.Lazy.IO as TIO
import Notes
import Parser
import Text.Megaparsec
import Text.Mustache

main :: IO ()
main = do
  --  putStrLn "Enter the directory name: "
  --  dir <- getLine
  --  renderDir dir
  let fileName = "notes-cs.md"
  let pattern = "##"
  --  let pattern = "------"

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

  tagTemplate <- compileMustacheFile "example/layouts/tags.mustache"
  --  print tagTemplate
  TIO.putStr $
    renderMustache tagTemplate $
      object
        ["tags" .= sortedUnique]
