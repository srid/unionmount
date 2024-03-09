{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}

module System.UnionMountSpec where

import Control.Monad.Logger.Extras (logToStderr, runLoggerLoggingT)
import Data.LVar qualified as LVar
import Data.Map.Strict qualified as Map
import System.FilePath ((</>))
import System.FilePattern (FilePattern)
import System.UnionMount qualified as UM
import Test.Hspec
import UnliftIO.Async (race_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (removeFile, withCurrentDirectory)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "unionmount" $ do
    it "basic" $ do
      unionMountSpec
        "basic"
        ( do
            writeFile "file1" "hello"
        )
        ( do
            writeFile "file1" "hello, again"
            writeFile "file2" "another file"
        )
        $ Map.fromList
          [ ("file1", "hello, again"),
            ("file2", "another file")
          ]
    it "deletion" $ do
      unionMountSpec
        "basic"
        ( do
            writeFile "file1" "hello"
            writeFile "file2" "another file"
        )
        ( do
            writeFile "file1" "hello, again"
            removeFile "file2"
        )
        $ Map.fromList
          [ ("file1", "hello, again")
          ]

-- | Test `UM.mount` using a set of IO operations, and checking the final result.
unionMountSpec ::
  -- | The name of the temporary directory for this test
  String ->
  -- | How to initialize the temporary directory
  IO () ->
  -- | IO operations to perform after setting up the union mount
  IO () ->
  -- | Final expected filesystem tree
  Map.Map FilePath ByteString ->
  Expectation
unionMountSpec name ini update expected = do
  -- Create a temporary directory, add a file to it, call `mount`, make an update to that file, and check that it is updated in memory.
  withSystemTempDirectory name $ \tempDir -> do
    withCurrentDirectory tempDir ini
    model <- LVar.empty
    flip runLoggerLoggingT logToStderr $ do
      (model0, patch) <- UM.mount tempDir allFiles ignoreNone mempty $ \() fp -> \case
        UM.Delete -> pure $ Map.delete fp
        UM.Refresh _ () -> do
          s <- readFileBS $ tempDir </> fp
          pure $ Map.insert fp s
      LVar.set model model0
      race_
        (patch $ LVar.set model)
        ( do
            -- NOTE: These timings may not be enough on a slow system.
            threadDelay 500_000 -- Wait for the initial model to be loaded.
            liftIO $ withCurrentDirectory tempDir update
            threadDelay 500_000 -- Wait for fsnotify to handle events
        )
    finalModel <- LVar.get model
    finalModel `shouldBe` expected

allFiles :: [((), FilePattern)]
allFiles = [((), "*")]

ignoreNone :: [a]
ignoreNone = []
