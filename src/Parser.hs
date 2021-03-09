{-# LANGUAGE ScopedTypeVariables #-}

module Parser
  ( convertFile,
    convertLines,
    getFile,
    findMarkdownFiles,
  )
where

import Commonmark
import Commonmark.Extensions.AutoIdentifiers (autoIdentifiersSpec)
import Commonmark.Extensions.Autolink (autolinkSpec)
import Commonmark.Extensions.ImplicitHeadingReferences (implicitHeadingReferencesSpec)
import Commonmark.Extensions.PipeTable (pipeTableSpec)
import qualified Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Internal as TI
import Data.Text.Lazy as TL
import Data.Text.Lazy.IO as TLIO
import System.Directory
import System.FilePath

-- TODO return only html?
getFile :: FilePath -> IO (Either ParseError (Html ()))
getFile f = do
  res <- TIO.readFile f
  convertHtml res

convertHtml :: TI.Text -> IO (Either ParseError (Html ()))
convertHtml x = do
  let spec = autoIdentifiersSpec <> defaultSyntaxSpec <> autolinkSpec <> pipeTableSpec <> implicitHeadingReferencesSpec
  parseCommonmarkWith spec (tokenize "source" x)

convertLines :: [TI.Text] -> IO (Either ParseError (Html ()))
convertLines x = do
  convertHtml (T.unlines x)  -- merge lines with "\n" character

convertFile :: Either ParseError (Html ()) -> TL.Text
convertFile h =
  case h of
    Left e -> error (show e)
    Right (ht :: Html ()) -> renderHtml ht

findMarkdownFiles :: FilePath -> IO [FilePath]
findMarkdownFiles f = do
  d <- listDirectory f
  let md = Prelude.filter (\x -> takeExtension x == ".md") d
  -- TODO recursively check any directories for more markdown files
  --  let dir = filter (\x -> doesDirectoryExist x) d
  mapM (\x -> makeAbsolute (f ++ x)) md

---- TODO this is temp
--renderDir :: FilePath -> IO [()]
--renderDir f = do
--  md <- findMarkdownFiles f
--  mapM convertFile (mapM getFile md)
