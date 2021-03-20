module ParserSpec (spec) where

import Huha.Parser
import Test.Hspec


spec :: Spec
spec =  describe "markdown.parser" $ do
      it "convert markdown to html" $ do
        print "not working yet"
--        convertFile "test-file.md" `shouldBe` ()

      it "find markdown files" $ do
        print "not working yet"
--        findMarkdownFiles "." `shouldBe` ["test-file.md"]
