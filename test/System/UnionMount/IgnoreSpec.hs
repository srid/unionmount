{-# LANGUAGE NumericUnderscores #-}

module System.UnionMount.IgnoreSpec where

import Data.Map.Strict qualified as Map
import System.FilePath ((</>))
import System.UnionMount.Ignore (readIgnoreFile)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "readIgnoreFile" $ do
    it "reads ignore patterns from a single file" $
      testIgnoreFile
        (Map.singleton "dir1" "*.tmp\n# comment\n\n*.log")
        ["*.log", "*.tmp"]

    it "combines patterns from multiple sources" $
      testIgnoreFile
        (Map.fromList [("dir1", "*.tmp"), ("dir2", "*.log")])
        ["*.log", "*.tmp"]

    it "skips sources without ignore file" $
      testIgnoreFile
        (Map.fromList [("dir1", "*.tmp"), ("dir2", "")])
        ["*.tmp"]

    it "filters out comments and empty lines" $
      testIgnoreFile
        (Map.singleton "dir1" "# This is a comment\n*.tmp\n\n# Another comment\n*.log\n\n")
        ["*.log", "*.tmp"]

    it "strips whitespace from patterns" $
      testIgnoreFile
        (Map.singleton "dir1" "  *.tmp  \n  *.log  ")
        ["*.log", "*.tmp"]

    it "returns empty list when no sources provided" $ do
      patterns <- readIgnoreFile ".ignore" []
      patterns `shouldBe` []

-- | Test helper that creates temp directories with ignore files and verifies the result
testIgnoreFile :: Map String String -> [String] -> IO ()
testIgnoreFile sourcesToContent expected = do
  let sources = Map.keys sourcesToContent
  withTempDirs (length sources) $ \dirs -> do
    let dirMap = Map.fromList $ zip sources dirs
    -- Write ignore files
    forM_ (Map.toList sourcesToContent) $ \(source, content) -> do
      let dir = dirMap Map.! source
      when (not $ null content) $
        writeFile (dir </> ".ignore") content
    -- Read and verify
    patterns <- readIgnoreFile ".ignore" dirs
    sort patterns `shouldBe` sort expected

-- Helper functions
withTempDirs :: Int -> ([FilePath] -> IO a) -> IO a
withTempDirs n action = go n []
  where
    go 0 acc = action (reverse acc)
    go count acc = withSystemTempDirectory "ignore-test" $ \dir ->
      go (count - 1) (dir : acc)
