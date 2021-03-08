{-# LANGUAGE OverloadedStrings #-}

-- :set -XOverloadedStrings

module Notes
  ( getLines,
    checkLine,
    printLines,
    parseHeader,
    parseTags,
    writeNotes,
    getUniqueTags,
    Header,
    tags,
  )
where

import Data.Aeson
import Data.List (nub, sort)
import Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Internal as TI
import Data.Text.Internal.Lazy as TIL
import qualified Data.Text.Lazy.IO as TLIO
import System.FilePath (joinPath)
import Text.Megaparsec
import Text.Mustache

type SplitPattern = TI.Text

type Tags = [T.Text]

type RawNote = [T.Text]

data Header = Header
  { rawHeader :: T.Text,
    date :: TI.Text,
    tags :: Tags,
    title :: T.Text
  }
  deriving (Show, Eq)

data Note = RawNote Header

--createNotes :: [TI.Text] -> SplitPattern -> [Note]
--createNotes lines pattern =
--  headers = checkLine lines pattern
--  map parseTags headers
--
getLines :: FilePath -> IO [T.Text]
getLines fileName = do
  fmap T.lines (TIO.readFile fileName)

checkLine :: [TI.Text] -> SplitPattern -> [TI.Text]
checkLine lines pattern =
  --  Prelude.filter (\line -> line == pattern) lines
  Prelude.filter (\line -> (isPrefixOf pattern line)) lines

-- | parseHeader splits the header into 3 sections:
--   1. date
--   2. tags
--   3. title
parseHeader :: T.Text -> Header
parseHeader t =
  let sections = T.splitOn " - " t
   in Header
        { rawHeader = t,
          date = parseDate (sections !! 0),
          tags = parseTags (sections !! 1),
          title = sections !! 2
        }

-- | parseDate strips the leading header level from Markdown and returns the
--   date. In the future, might do some more formatting on the date here.
parseDate :: T.Text -> T.Text
parseDate d =
  let splits = T.splitOn "## " d
   in splits !! 1

-- | parseTags takes a string of tags from a header and removes the
--  surrounding characters for each tag. It returns a clean list of
--  all the tags in the header.
--  e.g. "[[linux]] [[networking]]" will become ["linux","networking"]
--  e.g. "[[zsh]]" will become ["zsh"]
parseTags :: T.Text -> [T.Text]
parseTags t =
  let splits = T.splitOn " " t
   in Prelude.map (\tags -> T.dropWhileEnd (== ']') (T.dropWhile (== '[') tags)) splits

getUniqueTags :: [Header] -> [T.Text]
getUniqueTags headers =
  let tagsOnly = Prelude.map Notes.tags headers
      allTags = Prelude.concat tagsOnly
    in sort (nub allTags)

--cleanTags :: [[Text]] -> [Text]
--cleanTags t =
--  let joined = sort (nub (Prelude.concat parsedTags))

printLines :: [TI.Text] -> IO ()
printLines lines = do
  mapM_ TIO.putStrLn lines

writeNotes :: FilePath -> [T.Text] -> TIL.Text -> IO ()
writeNotes dir tags content = do
  template <- compileMustacheDir "tags" $ joinPath [dir, "layouts"]
  TLIO.writeFile
    (joinPath [dir, "public/static/", "tags.html"])
    $ renderMustache template $
      object
        [ "tags" .= tags,
          "content" .= content
        ]
