{-# LANGUAGE OverloadedStrings #-}

module NotesSpec (notesSpec) where

import Notes 
import Test.Hspec

notesSpec :: Spec
notesSpec = describe "notes parsing" $ do
  let pattern = "## "
  let headerLine = "## 20201104 - [[linux]] [[test]] - man page sections"
  let tags = parseTags headerLine
  -- TODO
--  let parsedHeader = Header {rawHeader = headerLine, date = "20201104", tags = tags, title = "man page sections"}
  let parsedHeader = parseHeader headerLine

  it "extract single header line" $ do
    filterLines ["## asdf", "nope", "# nope"] pattern `shouldBe` ["## asdf"]

  it "extract multiple header lines" $ do
    filterLines ["## asdf", "nope", "## yep"] pattern `shouldBe` ["## asdf", "## yep"]

  it "parse header - wip" $ do
    let a = parseHeader headerLine 
    print a
    a `shouldBe` parsedHeader

  it "extract single tag" $ do
    parseTags "[[linux]]" `shouldBe` ["linux"]

  it "extract multiple tags" $ do
    parseTags "[[linux]] [[networking]]" `shouldBe` ["linux", "networking"]
