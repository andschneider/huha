{-# LANGUAGE OverloadedStrings #-}

-- :set -XOverloadedStrings

module Notes
  ( getLines,
    checkLine,
    printLines,
    parseHeader,
    parseTags,
  )
where

import Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Internal as TI
import Data.List.NonEmpty (nub)

type SplitPattern = TI.Text

getLines :: FilePath -> IO [Text]
getLines fileName = do
  fmap T.lines (TIO.readFile fileName)

checkLine :: [TI.Text] -> SplitPattern -> [TI.Text]
checkLine lines pattern =
  --  Prelude.filter (\line -> line == pattern) lines
  Prelude.filter (\line -> (isPrefixOf pattern line)) lines

-- TODO parse all fields into record type?

parseHeader :: Text -> Text
parseHeader t =
  let sections = T.splitOn " - " t
   in sections !! 1 -- only return tags for now

-- | parseTags takes a string of tags from a header and removes the
--  surrounding characters for each tag. It returns a clean list of
--  all the tags in the header.
--  e.g. "[[linux]] [[networking]]" will become ["linux","networking"]
--  e.g. "[[zsh]]" will become ["zsh"]
parseTags :: Text -> [Text]
parseTags t =
  let splits = T.splitOn " " t
   in Prelude.map (\tags -> T.dropWhileEnd (== ']') (T.dropWhile (== '[') tags)) splits

--cleanTags :: [[Text]] -> [Text]
--cleanTags t =
  --  let joined = sort (nub (Prelude.concat parsedTags))

printLines :: [TI.Text] -> IO ()
printLines lines = do
  mapM_ TIO.putStrLn lines

--getCounts :: T.Text -> (Int, Int, Int)
--getCounts input = (charCount, wordCount, lineCount)
--  where
--    charCount = T.length input
--    wordCount = (Prelude.length . T.words) input
--    lineCount = (Prelude.length . T.lines) input
--
--countsText :: (Int, Int, Int) -> T.Text
--countsText (cc, wc, lc) =
--  T.pack
--    ( Prelude.unwords
--        [ "chars: ",
--          show cc,
--          " words: ",
--          show wc,
--          " lines: ",
--          show lc
--        ]
--    )
