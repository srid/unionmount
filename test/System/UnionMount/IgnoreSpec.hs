{-# LANGUAGE NumericUnderscores #-}

module System.UnionMount.IgnoreSpec where

import System.FilePath ((</>))
import System.UnionMount.Ignore (readIgnoreFile)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "readIgnoreFile" $ do
    it "reads ignore patterns from a single file" $ do
      withSystemTempDirectory "ignore-test" $ \dir -> do
        writeFile (dir </> ".gitignore") "*.tmp\n# comment\n\n*.log"
        patterns <- readIgnoreFile ".gitignore" [dir]
        sort patterns `shouldBe` sort ["*.log", "*.tmp"]

    it "combines patterns from multiple sources" $ do
      withSystemTempDirectory "ignore-test" $ \dir1 -> do
        withSystemTempDirectory "ignore-test" $ \dir2 -> do
          writeFile (dir1 </> ".ignore") "*.tmp"
          writeFile (dir2 </> ".ignore") "*.log"
          patterns <- readIgnoreFile ".ignore" [dir1, dir2]
          sort patterns `shouldBe` sort ["*.log", "*.tmp"]

    it "skips sources without ignore file" $ do
      withSystemTempDirectory "ignore-test" $ \dir1 -> do
        withSystemTempDirectory "ignore-test" $ \dir2 -> do
          writeFile (dir1 </> ".ignore") "*.tmp"
          -- dir2 has no .ignore file
          patterns <- readIgnoreFile ".ignore" [dir1, dir2]
          patterns `shouldBe` ["*.tmp"]

    it "filters out comments and empty lines" $ do
      withSystemTempDirectory "ignore-test" $ \dir -> do
        writeFile (dir </> ".ignore") "# This is a comment\n*.tmp\n\n# Another comment\n*.log\n\n"
        patterns <- readIgnoreFile ".ignore" [dir]
        sort patterns `shouldBe` sort ["*.log", "*.tmp"]

    it "strips whitespace from patterns" $ do
      withSystemTempDirectory "ignore-test" $ \dir -> do
        writeFile (dir </> ".ignore") "  *.tmp  \n  *.log  "
        patterns <- readIgnoreFile ".ignore" [dir]
        sort patterns `shouldBe` sort ["*.log", "*.tmp"]

    it "returns empty list when no sources provided" $ do
      patterns <- readIgnoreFile ".ignore" []
      patterns `shouldBe` []
