{-# LANGUAGE OverloadedStrings #-}

-- :set -XOverloadedStrings

module Notes
  ( getLines,
    checkLine,
    printLines,
    parseHeader,
    parseTags,
    writeNotes,
  )
where

import Data.Aeson
import Data.List.NonEmpty (nub)
import Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Internal as TI
import Data.Text.Internal.Lazy as TIL
import qualified Data.Text.Lazy.IO as TLIO
import System.FilePath (joinPath)
import Text.Megaparsec
import Text.Mustache

type SplitPattern = TI.Text

getLines :: FilePath -> IO [T.Text]
getLines fileName = do
  fmap T.lines (TIO.readFile fileName)

checkLine :: [TI.Text] -> SplitPattern -> [TI.Text]
checkLine lines pattern =
  --  Prelude.filter (\line -> line == pattern) lines
  Prelude.filter (\line -> (isPrefixOf pattern line)) lines

-- TODO parse all fields into record type?

parseHeader :: T.Text -> T.Text
parseHeader t =
  let sections = T.splitOn " - " t
   in sections !! 1 -- only return tags for now

-- | parseTags takes a string of tags from a header and removes the
--  surrounding characters for each tag. It returns a clean list of
--  all the tags in the header.
--  e.g. "[[linux]] [[networking]]" will become ["linux","networking"]
--  e.g. "[[zsh]]" will become ["zsh"]
parseTags :: T.Text -> [T.Text]
parseTags t =
  let splits = T.splitOn " " t
   in Prelude.map (\tags -> T.dropWhileEnd (== ']') (T.dropWhile (== '[') tags)) splits

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
    (joinPath [dir, "static/tags.html"])
    $ renderMustache template $
      object
        [ "tags" .= tags,
          "content" .= content
        ]
