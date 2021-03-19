{-# LANGUAGE OverloadedStrings #-}

module Huha.Build
  ( prepareBuild,
    runBuild,
    createConfig,
    Config,
  )
where

import qualified Data.Text as T
import Data.Text.Internal as TI (Text)
import Huha.Fsutils
import Huha.Notes
import Huha.Parser
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)

-- the location where all assets are held, default is "."
type BaseDir = FilePath

-- the name of the input file which is assumed to be in "BaseDir/content"
type InputFile = FilePath

-- the location where the "public" folder should be created. all built
-- files and static assets will be in this location
type OutputLocation = FilePath

data Config = Config
  { baseDir :: FilePath,
    noteFile :: InputFile,
    outputDir :: OutputLocation,
    headerPattern :: TI.Text,
    splitPattern :: TI.Text
  }

createConfig :: BaseDir -> InputFile -> OutputLocation -> TI.Text -> TI.Text -> Config
createConfig d i o hp sp =
  let notes = joinPath [d, "content", i]
   in Config {baseDir = d, noteFile = notes, outputDir = o, headerPattern = hp, splitPattern = sp}

-- | Create the output folders for static assets and the notes files.
prepareBuild :: Config -> IO ()
prepareBuild config = do
  let static = joinPath [outputDir config, "public/static"]
      notesDir = joinPath [outputDir config, "public/notes"]
   in mapM_ (createDirectoryIfMissing True) [notesDir, static]

-- | Run the full build, creating an index file and all the note files.
runBuild :: Config -> IO ()
runBuild config = do
  copyStaticAssets config
  -- read in the file to a list of text, with each line as a element in the list
  fileLines <- getLines (noteFile config) -- TODO use content dir
  let headers = Prelude.map parseHeader (filterLines fileLines (headerPattern config))
      sortedUnique = getUniqueTags headers
  buildIndex config fileLines sortedUnique
  mapM_ (writeBlankNote (baseDir config) (outputDir config)) sortedUnique -- create blank note files from a template
  buildNotes config fileLines -- append notes to their files

-- | Create the index.html file at "outputDir/public/index.html".
buildIndex :: Config -> [T.Text] -> Huha.Notes.Tags -> IO ()
buildIndex config lines tags = do
  parsed <- convertLines lines
  let html = convertFile parsed -- deal with the Either monad
   in writeNotes (baseDir config) (outputDir config) tags html

-- | Create the individual note files at outputDir/public/notes/<tag>.html.
buildNotes :: Config -> [T.Text] -> IO ()
buildNotes config lines = do
  -- super fragile way of removing the first two lines of the file
  -- would be better to remove until first match of pattern...
  let removeFirstTwo = tail (tail lines)
      notes = createNotes removeFirstTwo (splitPattern config)
   in mapM_ (appendNote (outputDir config)) notes

-- | Copy the static assets over to the public folder.
copyStaticAssets :: Config -> IO ()
copyStaticAssets config = do
  let staticFiles = joinPath [baseDir config, "static"]
      destination = joinPath [outputDir config, "public/static"]
   in copyDir staticFiles destination
