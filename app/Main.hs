{-# LANGUAGE OverloadedStrings #-}

module Main where

import Notes
import Parser

main :: IO ()
main = do
  let fileName = "notes-cs.md"
  let pattern = "##"
--  let pattern = "------"

  ls <- getLines fileName
  let lines = checkLine ls pattern
  let tags = map parseHeader lines -- TODO parse full header
  let t = map parseTags tags

  printLines lines
  printLines tags
  mapM_ printLines t

--  putStrLn "Enter the directory name: "
--  dir <- getLine
--  renderDir dir
