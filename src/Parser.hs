{-# LANGUAGE ScopedTypeVariables #-}
module Parser
  ( convertFile
  ) where

import Commonmark
import Data.Text.IO as TIO
import Data.Text.Internal as TI
import Data.Text.Lazy.IO as TLIO

convertFile :: FilePath -> IO ()
convertFile f = do
  res <- TIO.readFile f
  html <- convertHtml res
  case html of
    Left e                  -> error (show e)
    Right (html :: Html ()) -> TLIO.putStr $ renderHtml html

convertHtml :: TI.Text -> IO (Either ParseError (Html ()))
convertHtml x = do
  parseCommonmarkWith defaultSyntaxSpec (tokenize "source" x)
