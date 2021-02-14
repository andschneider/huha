{-# LANGUAGE ScopedTypeVariables #-}

module Parser
  ( convertFile,
    findMarkdownFiles,
    renderDir,
  )
where

import Commonmark
import Data.Text.IO as TIO
import Data.Text.Internal as TI
import Data.Text.Lazy.IO as TLIO
import System.Directory
import System.FilePath

convertFile :: FilePath -> IO ()
convertFile f = do
  res <- TIO.readFile f
  html <- convertHtml res
  case html of
    Left e -> error (show e)
    Right (h :: Html ()) -> TLIO.putStr $ renderHtml h

convertHtml :: TI.Text -> IO (Either ParseError (Html ()))
convertHtml x = do
  parseCommonmarkWith defaultSyntaxSpec (tokenize "source" x)

findMarkdownFiles :: FilePath -> IO [FilePath]
findMarkdownFiles f = do
  d <- listDirectory f
  let md = filter (\x -> takeExtension x == ".md") d
  -- TODO recursively check any directories for more markdown files
  --  let dir = filter (\x -> doesDirectoryExist x) d
  mapM (\x -> makeAbsolute (f ++ x)) md

-- TODO this is temp
renderDir :: FilePath -> IO [()]
renderDir f = do
  md <- findMarkdownFiles f
  mapM convertFile md
