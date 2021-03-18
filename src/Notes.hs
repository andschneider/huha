{-# LANGUAGE OverloadedStrings #-}

module Notes
  ( getLines,
    filterLines,
    printLines,
    parseHeader,
    parseTags,
    writeNotes,
    getUniqueTags,
    writeBlankNote,
    Header,
    tags,
    note,
    extractUntil,
    createNotes,
    appendNote,
  )
where

import Data.Aeson ( object, KeyValue((.=)) )
import Data.List (groupBy, nub, sort)
import qualified Data.Text as T
import Data.Text.IO as TIO ( putStrLn, readFile, writeFile )
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

data Header = Header
  { rawHeader :: T.Text,
    date :: TI.Text,
    tags :: Tags,
    title :: T.Text
  }
  deriving (Show, Eq)

data Note = Note
  { header :: Header,
    note :: RawNote
  }
  deriving (Show)

createNotes :: [T.Text] -> SplitPattern -> [Note]
createNotes ls pattern =
  -- use tail on the list to ignore the header in the file
  let extracted = extractUntil (\x -> not $ T.isPrefixOf pattern x) ls
   in map linesToNote extracted

linesToNote :: [T.Text] -> Note
linesToNote ls =
  let h = parseHeader (ls !! 1)
      rawNote = ls
   in Note {header = h, note = rawNote}

-- based on: https://stackoverflow.com/a/50068932
extractUntil :: (a -> Bool) -> [a] -> [[a]]
extractUntil _ [] = []
extractUntil p ls =
  let (ls', ls'') = span p ls
      remaining = dropWhile (not . p) ls''
   in case ls' of
        [] -> extractUntil p remaining
        xs' -> xs' : extractUntil p ls''

-- | read in a file and return a list of all the lines of the file.
getLines :: FilePath -> IO [T.Text]
getLines fileName = do
  fmap T.lines (TIO.readFile fileName)

-- | filter based on the prefix of a line.
filterLines :: [TI.Text] -> SplitPattern -> [TI.Text]
filterLines lines pattern =
  Prelude.filter (\line -> (T.isPrefixOf pattern line)) lines

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
   in Prelude.map (\tag -> T.dropWhileEnd (== ']') (T.dropWhile (== '[') tag)) splits

-- | extract all unique tags from a list of headers.
getUniqueTags :: [Header] -> [T.Text]
getUniqueTags headers =
  let tagsOnly = Prelude.map Notes.tags headers
      allTags = Prelude.concat tagsOnly
   in sort (nub allTags)

-- | write all of the tags and converted markdown to a html file
writeNotes :: FilePath -> [T.Text] -> TIL.Text -> IO ()
writeNotes dir tags content = do
  template <- compileMustacheDir "main" $ joinPath [dir, "layouts"]
  TLIO.writeFile
    (joinPath [dir, "public/static/", "index.html"])
    $ renderMustache template $
      object
        [ "tags" .= tags,
          "content" .= content
        ]

-- | append a note to its specific note file, based on its tag
appendNote :: FilePath -> Note -> IO ()
appendNote dir n = do
  html <- convertLines (note n)
  let render = convertFile html
    -- map over all the tags in a note, duplicating the note if it has more than one tag
    in mapM_ (appendNote' dir render) (tags (header n))

-- | recursive helper to append note to a html file
appendNote' :: FilePath -> TIL.Text -> T.Text -> IO ()
appendNote' dir content t = do
  let fn = T.unpack (mconcat [t, ".html"])
   in TLIO.appendFile (joinPath [dir, "public/static/notes/", fn]) content

-- | create a blank html file based on the name of a tag.
--  the file should be appended to later with actual content.
writeBlankNote :: FilePath -> T.Text -> IO ()
writeBlankNote dir tag = do
  let fn = T.unpack (mconcat [tag, ".html"])
  template <- compileMustacheDir "category" $ joinPath [dir, "layouts"]
  TLIO.writeFile
    (joinPath [dir, "public/static/notes", fn])
    $ renderMustache template $
      object
        [ "tag" .= tag ]

printLines :: [TI.Text] -> IO ()
printLines lines = do
  mapM_ TIO.putStrLn lines
