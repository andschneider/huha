{-# LANGUAGE ScopedTypeVariables #-}

module Parser
  ( convertFile,
    convertLines,
    getFile,
    findMarkdownFiles,
  )
where

import Commonmark
    ( Html,
      ParseError,
      renderHtml,
      parseCommonmarkWith,
      defaultSyntaxSpec,
      tokenize )
import Commonmark.Extensions.Autolink (autolinkSpec)
import Commonmark.Extensions.ImplicitHeadingReferences (implicitHeadingReferencesSpec)
import Commonmark.Extensions.PipeTable (pipeTableSpec)
import qualified Data.Text as T
import Data.Text.IO as TIO ( readFile )
import Data.Text.Internal as TI ( Text )
import Data.Text.Lazy as TL ( Text )
import System.Directory ( listDirectory, makeAbsolute )
import System.FilePath ( takeExtension )

-- | Read in a markdown file and attempt to convert it to html.
getFile :: FilePath -> IO (Either ParseError (Html ()))
getFile f = do
  res <- TIO.readFile f
  convertHtml res -- TODO return only html?

-- | Parse the text into html using commonmark-hs. The extensions defined in
-- the `spec` can be found here: https://github.com/jgm/commonmark-hs/tree/master/commonmark-extensions
convertHtml :: TI.Text -> IO (Either ParseError (Html ()))
convertHtml x = do
  let spec = defaultSyntaxSpec <> autolinkSpec <> pipeTableSpec <> implicitHeadingReferencesSpec
  parseCommonmarkWith spec (tokenize "source" x)

-- | Helper function to convert a list of lines to html.
convertLines :: [TI.Text] -> IO (Either ParseError (Html ()))
convertLines x = do
  convertHtml (T.unlines x)  -- merge lines with "\n" character

-- | Handle the Either monad and return the html as text.
convertFile :: Either ParseError (Html ()) -> TL.Text
convertFile h =
  case h of
    Left e -> error (show e)
    Right (ht :: Html ()) -> renderHtml ht

-- TODO not used
findMarkdownFiles :: FilePath -> IO [FilePath]
findMarkdownFiles f = do
  d <- listDirectory f
  let md = Prelude.filter (\x -> takeExtension x == ".md") d
  -- TODO recursively check any directories for more markdown files
  --  let dir = filter (\x -> doesDirectoryExist x) d
  mapM (\x -> makeAbsolute (f ++ x)) md
