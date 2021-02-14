module Main where

import Parser

main :: IO [()]
main = do
  putStrLn "Enter the directory name: "
  dir <- getLine
  renderDir dir
