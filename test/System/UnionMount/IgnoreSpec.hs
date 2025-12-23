{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}

module System.UnionMount.IgnoreSpec where

import NeatInterpolation (text)
import System.FilePath ((</>))
import System.UnionMount.Ignore (IgnorePattern (..), applyIgnorePatterns, readIgnoreFile)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "readIgnoreFile" do
    it "reads ignore patterns from a file" $
      withSystemTempDirectory "ignore-test" \dir -> do
        let ignoreFile = dir </> ".emanoteignore"
        writeFile ignoreFile $
          toString
            [text|
          *.tmp

          # ignore log files
          *.log

        |]
        patterns <- readIgnoreFile ignoreFile
        patterns `shouldBe` [Ignore "*.tmp", Ignore "*.log"]

    it "parses negation patterns" $
      withSystemTempDirectory "ignore-test" \dir -> do
        let ignoreFile = dir </> ".emanoteignore"
        writeFile ignoreFile $
          toString
            [text|
          *.tmp
          !important.tmp
          *.log
        |]
        patterns <- readIgnoreFile ignoreFile
        patterns `shouldBe` [Ignore "*.tmp", UnIgnore "important.tmp", Ignore "*.log"]

    it "returns empty list when file doesn't exist" $
      withSystemTempDirectory "ignore-test" \dir -> do
        let ignoreFile = dir </> ".emanoteignore"
        patterns <- readIgnoreFile ignoreFile
        patterns `shouldBe` []

  describe "applyIgnorePatterns" do
    it "appends Ignore patterns" do
      let result = applyIgnorePatterns [".*"] [Ignore "*.tmp"]
      result `shouldBe` [".*", "*.tmp"]

    it "removes patterns with UnIgnore" do
      let result = applyIgnorePatterns [".*", "*.tmp"] [UnIgnore ".*"]
      result `shouldBe` ["*.tmp"]

    it "combines Ignore and UnIgnore" do
      let result = applyIgnorePatterns [".*"] [UnIgnore ".*", Ignore "*.tmp"]
      result `shouldBe` ["*.tmp"]

    it "handles empty global patterns" do
      let result = applyIgnorePatterns [] [Ignore "*.tmp", Ignore "*.log"]
      result `shouldBe` ["*.tmp", "*.log"]

    it "handles empty local patterns" do
      let result = applyIgnorePatterns [".*", "*.tmp"] []
      result `shouldBe` [".*", "*.tmp"]
