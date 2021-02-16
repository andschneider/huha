{-# LANGUAGE OverloadedStrings #-}

module NotesSpec (notesSpec) where

import Notes
import Test.Hspec

notesSpec :: Spec
notesSpec = describe "notes parsing" $ do
  let pattern = "## "
  let headerLine = "## 20201104 - [[linux]] - man page sections"

  it "extract single header line" $ do
    checkLine ["## asdf", "nope", "# nope"] pattern `shouldBe` ["## asdf"]

  it "extract multiple header lines" $ do
    checkLine ["## asdf", "nope", "## yep"] pattern `shouldBe` ["## asdf", "## yep"]

  it "parse header - wip" $ do
    parseHeader headerLine `shouldBe` "[[linux]]"

  it "extract single tag" $ do
    parseTags "[[linux]]" `shouldBe` ["linux"]

  it "extract multiple tags" $ do
    parseTags "[[linux]] [[networking]]" `shouldBe` ["linux", "networking"]
