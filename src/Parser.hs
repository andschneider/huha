{-# LANGUAGE ScopedTypeVariables #-}
module Parser
  ( convertFile
  ) where

import Commonmark
import Data.Text.IO as TIO
import Data.Text.Internal as TI
import Data.Text.Lazy.IO as TLIO

convertFile :: IO ()
convertFile = do
  res <- TIO.readFile "README.md"
  html <- convertHtml res
  case html of
    Left e                  -> error (show e)
    Right (html :: Html ()) -> TLIO.putStr $ renderHtml html

convertHtml :: TI.Text -> IO (Either ParseError (Html ()))
convertHtml text = do
  parseCommonmarkWith defaultSyntaxSpec (tokenize "source" text)
