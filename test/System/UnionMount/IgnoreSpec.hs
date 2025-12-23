{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}

module System.UnionMount.IgnoreSpec where

import Data.Map.Strict qualified as Map
import Data.Map.Syntax (MapSyntax, runMap, (##))
import NeatInterpolation (text)
import System.FilePath ((</>))
import System.UnionMount.Ignore (readIgnoreFile)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "readIgnoreFile" $ do
    it "reads ignore patterns from a single file" $
      testIgnoreFile ["*.log", "*.tmp"] do
        "dir1" ## [text|
          *.tmp

          # ignore log files
          *.log

        |]

    it "combines patterns from multiple sources" $
      testIgnoreFile ["*.log", "*.tmp"] do
        "dir1" ## "*.tmp"
        "dir2" ## "*.log"

    it "skips sources without ignore file" $
      testIgnoreFile ["*.tmp"] do
        "dir1" ## "*.tmp"
        "dir2" ## ""

    it "returns empty list when no sources provided" $
      testIgnoreFile [] mempty

-- | Test helper that creates temp directories with ignore files and verifies the result
testIgnoreFile :: [String] -> MapSyntax Text Text -> IO ()
testIgnoreFile expected mapSyntax = case runMap mapSyntax of
  Left dups -> expectationFailure $ "Duplicate keys: " <> show dups
  Right sourcesToContent -> do
    let sources = Map.keys sourcesToContent
    withTempDirs (length sources) $ \dirs -> do
      let dirMap = Map.fromList $ zip sources dirs
      -- Write ignore files
      forM_ (Map.toList sourcesToContent) $ \(source, content) -> do
        let dir = dirMap Map.! source
        when (content /= "") $
          writeFile (dir </> ".ignore") (toString content)
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
