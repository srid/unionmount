{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}

module System.UnionMountSpec where

import Control.Monad.Logger.Extras (logToStderr, runLoggerLoggingT)
import Data.LVar qualified as LVar
import Data.List (stripPrefix, union)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Traversable (for)
import Relude.Unsafe qualified as Unsafe
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath ((</>))
import System.FilePattern (FilePattern)
import System.UnionMount qualified as UM
import Test.Hspec
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (race_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (removeFile, withCurrentDirectory)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  -- TODO: Use QuickCheck to generate these.
  describe "unionmount" $ do
    it "basic" $ do
      unionMountSpec $
        one $
          FolderMutation
            ( do
                writeFile "file1" "hello"
            )
            ( do
                writeFile "file1" "hello, again"
                writeFile "file2" "another file"
            )
    it "deletion" $ do
      unionMountSpec $
        one $
          FolderMutation
            ( do
                writeFile "file1" "hello"
                writeFile "file2" "another file"
            )
            ( do
                writeFile "file1" "hello, again"
                removeFile "file2"
            )
    it "multiple layers" $ do
      unionMountSpec $
        FolderMutation
          ( do
              writeFile "file1" "hello"
              writeFile "file3" "hello"
          )
          ( do
              writeFile "file1" "hello, again"
          )
          :| [ FolderMutation
                 ( do
                     writeFile "file2" "another file"
                 )
                 ( do
                     writeFile "file2" "another file, again"
                     writeFile "file3" "file3 is in first layer"
                 )
             ]

-- | Test `UM.unionMount` on a set of folders whose contents/mutations are
-- represented by a `FolderMutation`, and check that the resulting model is
-- equivalent to the state when these mutations are applied in normal IO context
-- (outside of unionmount).
unionMountSpec ::
  -- | The folder mutations to test
  UnionFolderMutations ->
  Expectation
unionMountSpec folders = do
  withUnionFolderMutations folders $ \tempDirs -> do
    model <- LVar.empty
    flip runLoggerLoggingT logToStderr $ do
      let layers = Set.fromList $ toList tempDirs <&> \(_, path) -> (path, path)
      (model0, patch) <- UM.unionMount layers allFiles ignoreNone mempty $ \change -> do
        let files = Unsafe.fromJust $ Map.lookup () change
        flip UM.chainM (Map.toList files) $ \(fp, act) -> do
          case act of
            UM.Delete -> pure $ Map.delete fp
            UM.Refresh _ layerFiles -> do
              contents <- for layerFiles $ \(tempDir, path) ->
                readFileBS $ tempDir </> path
              pure $ Map.insert fp contents
      LVar.set model model0
      race_
        (patch $ LVar.set model)
        (withPaddedThreadDelay 500_000 $ updateUnionFolderMutations tempDirs)
    finalModel <- LVar.get model
    expected <- runUnionFolderMutations folders
    finalModel `shouldBe` expected
    print expected
  where
    -- NOTE: These timings may not be enough on a slow system.
    withPaddedThreadDelay :: (MonadUnliftIO m) => Int -> m () -> m ()
    withPaddedThreadDelay padding action = do
      -- Wait for the initial model to be loaded.
      threadDelay padding
      action
      -- Wait for fsnotify to handle events
      threadDelay padding

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

-- | A non-empty list of folder mutations that are meant to be unioned together.
type UnionFolderMutations = NonEmpty FolderMutation

runUnionFolderMutations :: UnionFolderMutations -> IO (Map.Map FilePath (NonEmpty ByteString))
runUnionFolderMutations folders =
  Map.unionsWith (<>) . fmap (Map.map one) <$> traverse runFolderMutation folders

-- | Create a temp directory for each folder in the list, and call the handler.
--
-- Also initialize each folders. Use `updateUnionFolderMutations` to update the
-- folders. And `runUnionFolderMutations` to get the final state of the folders,
-- with values unioned as lists.
withUnionFolderMutations ::
  (MonadUnliftIO m) =>
  UnionFolderMutations ->
  (NonEmpty (FolderMutation, FilePath) -> m a) ->
  m a
withUnionFolderMutations folders f = do
  withSystemTempDirectories folders $ \tempDirs -> do
    forM_ tempDirs $ \(folder, tempDir) ->
      liftIO $ withCurrentDirectory tempDir $ _folderMutationInit folder
    f tempDirs

updateUnionFolderMutations ::
  (MonadUnliftIO m) =>
  NonEmpty (FolderMutation, FilePath) ->
  m ()
updateUnionFolderMutations tempDirs = do
  forM_ tempDirs $ \(folder, tempDir) ->
    liftIO $ withCurrentDirectory tempDir $ _folderMutationUpdate folder

-- | Like `withSystemTempDirectory`, but for multiple temp directories.
withSystemTempDirectories ::
  (MonadUnliftIO m) =>
  -- | Create a temp directory for each tag in this list.
  NonEmpty tag ->
  -- | The handler is passed the temp directory along with the associated tag.
  (NonEmpty (tag, FilePath) -> m a) ->
  m a
withSystemTempDirectories = go mempty
  where
    go acc (tag :| []) f =
      withSystemTempDirectory "withSystemTempDirectories" $ \dir ->
        f $ NE.reverse $ (tag, dir) :| acc
    go acc (tag :| (t : ts)) f =
      withSystemTempDirectory "withSystemTempDirectories" $ \dir ->
        go ((tag, dir) : acc) (t :| ts) f

allFiles :: [((), FilePattern)]
allFiles = [((), "*")]

ignoreNone :: [a]
ignoreNone = []
