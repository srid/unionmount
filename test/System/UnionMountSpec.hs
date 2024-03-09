{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}

module System.UnionMountSpec where

import Control.Monad.Logger.Extras (logToStderr, runLoggerLoggingT)
import Data.LVar qualified as LVar
import Data.List (stripPrefix)
import Data.Map.Strict qualified as Map
import Relude.Unsafe qualified as Unsafe
import System.Directory.Recursive (getFilesRecursive)
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
        $ FolderMutation
          ( do
              writeFile "file1" "hello"
          )
          ( do
              writeFile "file1" "hello, again"
              writeFile "file2" "another file"
          )
    it "deletion" $ do
      unionMountSpec
        "basic"
        $ FolderMutation
          ( do
              writeFile "file1" "hello"
              writeFile "file2" "another file"
          )
          ( do
              writeFile "file1" "hello, again"
              removeFile "file2"
          )

-- | Represent the mutation of a folder over time.
--
-- Initial state of the folder, along with the mutations to perform, both as IO
-- actions.
data FolderMutation = FolderMutation
  { -- | How to initialize the folder
    _folderMutationInit :: IO (),
    -- | IO operations to perform for updating the folder
    _folderMutationUpdate :: IO ()
  }

runFolderMutation :: FolderMutation -> IO (Map.Map FilePath ByteString)
runFolderMutation folder = do
  withSystemTempDirectory "runFolderMutation" $ \tempDir -> do
    withCurrentDirectory tempDir $ do
      _folderMutationInit folder
      _folderMutationUpdate folder
      files <- getFilesRecursiveCurrentDir
      Map.fromList <$> forM files (\f -> (f,) <$> readFileBS f)
  where
    getFilesRecursiveCurrentDir :: IO [FilePath]
    getFilesRecursiveCurrentDir = do
      fs <- getFilesRecursive "."
      -- Remove the leading "./" from the file paths
      pure $ fs <&> \f -> fromMaybe f $ stripPrefix "./" f

-- | Test `UM.mount` using a set of IO operations, and checking the final result.
unionMountSpec ::
  -- | The name of the temporary directory for this test
  String ->
  -- | The folder mutation to test
  FolderMutation ->
  Expectation
unionMountSpec name folder = do
  -- Create a temporary directory, add a file to it, call `mount`, make an update to that file, and check that it is updated in memory.
  withSystemTempDirectory name $ \tempDir -> do
    withCurrentDirectory tempDir $ _folderMutationInit folder
    model <- LVar.empty
    flip runLoggerLoggingT logToStderr $ do
      (model0, patch) <- UM.unionMount (one ((), tempDir)) allFiles ignoreNone mempty $ \change -> do
        let files = Unsafe.fromJust $ Map.lookup () change
        flip UM.chainM (Map.toList files) $ \(fp, act) -> do
          case act of
            UM.Delete -> pure $ Map.delete fp
            UM.Refresh _ layers -> do
              let fpL = snd . last $ layers
              s <- readFileBS $ tempDir </> fpL
              pure $ Map.insert fp s
      LVar.set model model0
      race_
        (patch $ LVar.set model)
        ( do
            -- NOTE: These timings may not be enough on a slow system.
            threadDelay 500_000 -- Wait for the initial model to be loaded.
            liftIO $ withCurrentDirectory tempDir $ _folderMutationUpdate folder
            threadDelay 500_000 -- Wait for fsnotify to handle events
        )
    finalModel <- LVar.get model
    expected <- runFolderMutation folder
    finalModel `shouldBe` expected

allFiles :: [((), FilePattern)]
allFiles = [((), "*")]

ignoreNone :: [a]
ignoreNone = []
