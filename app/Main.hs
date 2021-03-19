{-# LANGUAGE OverloadedStrings #-}

module Main where

import Build

main :: IO ()
main = do
  -- TODO specific these as CLI inputs
  let input = "./example"
  let output = "./output2"
  let fileName = "notes-cs.md"
  --  let fileName = "/Users/andrew/akb-1/notes-cs.md"
  let headerPattern = "## 20"
  let splitPattern = "------"

  -- CONFIG:
  -- 1. notes files
  -- 2. output base directory
  -- 3. header pattern
  -- 4. split pattern

  let config = createConfig input fileName output headerPattern splitPattern
  prepareBuild config
  runBuild config
