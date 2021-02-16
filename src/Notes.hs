{-# LANGUAGE OverloadedStrings #-}

module Notes
  ( getLines,
    checkLine,
    printLines,
  )
where

import Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Internal as TI

type SplitPattern = TI.Text

getLines :: FilePath -> IO [Text]
getLines fileName = do
  fmap T.lines (TIO.readFile fileName)

checkLine :: [TI.Text] -> SplitPattern -> [TI.Text]
checkLine lines pattern =
  Prelude.filter (\line -> line == pattern) lines

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
