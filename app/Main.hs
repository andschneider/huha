{-# LANGUAGE OverloadedStrings #-}

module Main where

import Notes
import Parser

main :: IO ()
main = do
  let fileName = "notes-cs.md"
  let pattern = "------"

  ls <- getLines fileName
  let lines = checkLine ls pattern
  printLines lines

--  putStrLn "Enter the directory name: "
--  dir <- getLine
--  renderDir dir
