{-# LANGUAGE OverloadedStrings #-}

module Notes
  ( getLines,
    filterLines,
    parseHeader,
    parseTags,
    writeNotes,
    getUniqueTags,
    writeBlankNote,
    Header,
    Tags,
    tags,
    note,
    extractUntil,
    createNotes,
    appendNote,
  )
where

import Data.Aeson ( object, KeyValue((.=)) )
import Data.List (nub, sort)
import qualified Data.Text as T
import Data.Text.IO as TIO ( readFile )
import Data.Text.Internal as TI ( Text )
import Data.Text.Internal.Lazy as TIL ( Text )
import qualified Data.Text.Lazy.IO as TLIO
import Parser ( convertLines, convertFile )
import System.FilePath (joinPath)
import Text.Megaparsec ()
import Text.Mustache ( compileMustacheDir, renderMustache )

type SplitPattern = TI.Text

type Tags = [T.Text]

type RawNote = [T.Text]

data Header = Header { 
      rawHeader :: T.Text
    , date :: TI.Text
    , tags :: Tags
    , title :: T.Text
  } deriving (Show, Eq)

data Note = Note {
      header :: Header
    , note :: RawNote
  } deriving (Show)

-- | read in a file and return a list of all the lines of the file.
getLines :: FilePath -> IO [T.Text]
getLines fileName = do
  fmap T.lines (TIO.readFile fileName)

-- | filter based on the prefix of a line.
filterLines :: [TI.Text] -> SplitPattern -> [TI.Text]
filterLines lines pattern =
  Prelude.filter (T.isPrefixOf pattern) lines

-- | createNotes parses the lines of the note file and extracts each note,
--   which is in between two instances of the SplitPattern.
createNotes :: [T.Text] -> SplitPattern -> [Note]
createNotes ls pattern =
  -- use tail on the list to ignore the header in the file
  let extracted = extractUntil (not . T.isPrefixOf pattern) ls
   in map linesToNote extracted

-- | convert the text lines to a Note data type
linesToNote :: [T.Text] -> Note
linesToNote ls =
  -- the header is the 2nd line after the split pattern, the first is a blank line
  let h = parseHeader (ls !! 1)
      rawNote = ls
   in Note {header = h, note = rawNote}

-- | extractUntil is used to pull out the lines of text until a pattern match
--   is found. It is based on: https://stackoverflow.com/a/50068932
extractUntil :: (a -> Bool) -> [a] -> [[a]]
extractUntil _ [] = []
extractUntil p ls =
  let (ls', ls'') = span p ls
      remaining = dropWhile (not . p) ls''
   in case ls' of
        [] -> extractUntil p remaining
        xs' -> xs' : extractUntil p ls''

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
parseDate = head . tail . T.splitOn "## "

-- | parseTags takes a string of tags from a header and removes the
--  surrounding characters for each tag. It returns a clean list of
--  all the tags in the header.
--  e.g. "[[linux]] [[networking]]" will become ["linux","networking"]
--  e.g. "[[zsh]]" will become ["zsh"]
parseTags :: T.Text -> Tags
parseTags t =
  let splits = T.splitOn " " t
   in Prelude.map (T.dropWhileEnd (== ']') . T.dropWhile (== '[')) splits

-- | Extract all unique tags from a list of headers.
getUniqueTags :: [Header] -> Tags
getUniqueTags headers =
  let tagsOnly = Prelude.map Notes.tags headers
      allTags = Prelude.concat tagsOnly
   in sort (nub allTags)

-- | Write all of the tags and converted markdown to a file. This html file
--   becomes the base of the static site, as such, it is saved to "index.html".
writeNotes :: FilePath -> FilePath -> Tags -> TIL.Text -> IO ()
writeNotes input output t content = do
  template <- compileMustacheDir "main" $ joinPath [input, "layouts"]
  TLIO.writeFile
    (joinPath [output, "public/", "index.html"])
    $ renderMustache template $
      object
        [ "tags" .= t,
          "content" .= content
        ]

-- | Append a note to its specific note file, based on its tag.
appendNote :: FilePath -> Note -> IO ()
appendNote dir n = do
  html <- convertLines (note n)
  let render = convertFile html
    -- map over all the tags in a note, duplicating the note if it has more
    -- than one tag
    in mapM_ (appendNote' dir render) (tags (header n))

-- | Recursive helper to append note to a html file
appendNote' :: FilePath -> TIL.Text -> T.Text -> IO ()
appendNote' dir content t = do
  let fn = T.unpack (mconcat [t, ".html"])
   in TLIO.appendFile (joinPath [dir, "public/notes/", fn]) content

-- | Create a blank html file based on the name of a tag. The file should be
-- appended to later with actual content.
writeBlankNote :: FilePath -> FilePath -> T.Text -> IO ()
writeBlankNote input output tag = do
  let fn = T.unpack (mconcat [tag, ".html"])
  template <- compileMustacheDir "category" $ joinPath [input, "layouts"]
  TLIO.writeFile
    (joinPath [output, "public/notes", fn])
    $ renderMustache template $
      object
        [ "tag" .= tag ]
