{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}

module System.UnionMountSpec where

import Control.Monad.Logger.Extras (logToNowhere, runLoggerLoggingT)
import Data.LVar qualified as LVar
import Data.List (stripPrefix)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Traversable (for)
import Relude.Unsafe qualified as Unsafe
import System.Directory (createDirectory)
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath ((</>))
import System.FilePattern (FilePattern, (?==))
import System.UnionMount qualified as UM
import Test.Hspec
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (race_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (removeFile, withCurrentDirectory)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "unionmount" $ do
    it "basic" $ do
      unionMountSpec $
        one $
          FolderMutation
            Nothing
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
            Nothing
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
          Nothing
          ( do
              writeFile "file1" "hello"
              writeFile "file3" "hello"
          )
          ( do
              writeFile "file1" "hello, again"
          )
          :| [ FolderMutation
                 Nothing
                 ( do
                     writeFile "file2" "another file"
                 )
                 ( do
                     writeFile "file2" "another file, again"
                     writeFile "file3" "file3 is in first layer"
                 )
             ]
    it "mount point layers" $ do
      unionMountSpec $
        FolderMutation
          Nothing
          ( do
              writeFile "file1" "hello"
              writeFile "file3" "hello"
          )
          ( do
              writeFile "file1" "hello, again"
          )
          :| [ FolderMutation
                 (Just "foo")
                 ( do
                     writeFile "file2" "another file"
                 )
                 ( do
                     writeFile "file2" "another file, again"
                     writeFile "file3" "file3 is in first layer"
                 )
             ]
    it "ignore patterns exclude matching files" $ do
      unionMountSpecWith ["*.ignored"] $
        one $
          FolderMutation
            Nothing
            ( do
                writeFile "keep.txt" "keep me"
                writeFile "drop.ignored" "skip me"
            )
            ( do
                writeFile "keep.txt" "keep me, again"
                writeFile "also-drop.ignored" "also skip me"
            )
    it "delete from top layer reveals lower layer" $ do
      unionMountSpec $
        FolderMutation
          Nothing
          ( do
              writeFile "shared" "from lower"
          )
          ( pass
          )
          :| [ FolderMutation
                 Nothing
                 ( do
                     writeFile "shared" "from upper"
                 )
                 ( do
                     removeFile "shared"
                 )
             ]
  describe "unionMount tags" $ do
    it "initial scan partitions files by tag pattern" $ do
      withSystemTempDirectory "multitag" $ \dir -> do
        writeFile (dir </> "a.md") "md content"
        writeFile (dir </> "b.txt") "txt content"
        writeFile (dir </> "c.other") "ignored by patterns"
        let layers = Set.singleton (dir :: FilePath, (dir, Nothing))
            pats = [("md" :: String, "*.md"), ("txt", "*.txt")]
        (model0, _patch) <- flip runLoggerLoggingT logToNowhere $
          UM.unionMount layers pats mempty (mempty :: Map String (Set FilePath)) $ \change ->
            pure $ \m0 ->
              Map.foldrWithKey
                (\tag files -> Map.insertWith Set.union tag (Map.keysSet files))
                m0
                change
        model0
          `shouldBe` Map.fromList
            [ ("md", Set.singleton "a.md"),
              ("txt", Set.singleton "b.txt")
            ]
    it "per-source ignore patterns are scoped to their source" $ do
      withSystemTempDirectory "perSourceA" $ \dirA ->
        withSystemTempDirectory "perSourceB" $ \dirB -> do
          writeFile (dirA </> "secret.txt") "from A"
          writeFile (dirA </> "public.txt") "A is public"
          writeFile (dirB </> "secret.txt") "from B"
          let layers =
                Set.fromList
                  [ ("A" :: String, (dirA, Nothing)),
                    ("B", (dirB, Nothing))
                  ]
              pats = [((), "*.txt")]
              perSource = Map.singleton "A" ["secret.txt"]
          (model0, _patch) <- flip runLoggerLoggingT logToNowhere $
            UM.unionMount layers pats perSource (mempty :: Map FilePath [String]) $ \change ->
              pure $ foldChangeBySource change
          Map.lookup "secret.txt" model0 `shouldBe` Just ["B"]
          Map.lookup "public.txt" model0 `shouldBe` Just ["A"]
    it "per-source ignore patterns also gate live fsnotify events" $ do
      -- Same shape as above, but the ignored file is created *after* the
      -- mount is live, exercising the fsnotify loop's ignoreFor check
      -- rather than the initial directory scan.
      withSystemTempDirectory "perSourceLiveA" $ \dirA ->
        withSystemTempDirectory "perSourceLiveB" $ \dirB -> do
          let layers =
                Set.fromList
                  [ ("A" :: String, (dirA, Nothing)),
                    ("B", (dirB, Nothing))
                  ]
              pats = [((), "*.txt")]
              perSource = Map.singleton "A" ["secret.txt"]
          model <- LVar.empty
          flip runLoggerLoggingT logToNowhere $ do
            (model0, patch) <-
              UM.unionMount layers pats perSource (mempty :: Map FilePath [String]) $
                \change -> pure $ foldChangeBySource change
            LVar.set model model0
            race_
              (patch $ LVar.set model)
              ( do
                  threadDelay fsnotifyWatcherSetupDelay
                  writeFile (dirA </> "secret.txt") "from A (live)"
                  writeFile (dirA </> "public.txt") "A is public (live)"
                  writeFile (dirB </> "secret.txt") "from B (live)"
                  let target =
                        Map.fromList
                          [ ("secret.txt", ["B"]),
                            ("public.txt", ["A"])
                          ]
                  _ <- waitForModel model target
                  pass
              )
          finalModel <- LVar.get model
          Map.lookup "secret.txt" finalModel `shouldBe` Just ["B"]
          Map.lookup "public.txt" finalModel `shouldBe` Just ["A"]

-- | Test `UM.unionMount` on a set of folders whose contents/mutations are
-- represented by a `FolderMutation`, and check that the resulting model is
-- equivalent to the state when these mutations are applied in normal IO context
-- (outside of unionmount).
unionMountSpec ::
  -- | The folder mutations to test
  UnionFolderMutations ->
  Expectation
unionMountSpec = unionMountSpecWith ignoreNone

-- | Like `unionMountSpec`, but with a caller-supplied ignore list that is
-- passed to `UM.unionMount` and used to filter the IO-materialized expected
-- model.
unionMountSpecWith ::
  [FilePattern] ->
  UnionFolderMutations ->
  Expectation
unionMountSpecWith ignore folders = do
  -- Compute the expected final state once, using separate temp directories.
  rawExpected <- runUnionFolderMutations folders
  let expected = Map.filterWithKey (\fp _ -> not (any (?== fp) ignore)) rawExpected
  withUnionFolderMutations folders $ \tempDirs -> do
    model <- LVar.empty
    flip runLoggerLoggingT logToNowhere $ do
      let layers = Set.fromList $ toList tempDirs <&> \(folder, path) -> (path, (path, _folderMountPoint folder))
          perSourceIgnore = Map.fromSet (const ignore) (Set.map fst layers)
      (model0, patch) <- UM.unionMount layers allFiles perSourceIgnore mempty $ \change -> do
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
        ( do
            -- The fsnotify watcher is set up lazily when the sender
            -- (left side of `race_`) is invoked, so we give it a brief
            -- head start before mutating files.
            threadDelay fsnotifyWatcherSetupDelay
            updateUnionFolderMutations tempDirs
            -- Poll the model until it reflects the expected final state
            -- (or we hit the timeout ceiling).
            _ <- waitForModel model expected
            pass
        )
    finalModel <- LVar.get model
    finalModel `shouldBe` expected

-- | Project a `UM.Change source ()` into a flat `Map FilePath [source]`
-- — one entry per resulting filepath, listing the sources that contribute
-- to it. Used by the per-source-ignore tests to assert which layers a
-- given path is sourced from after the mount converges.
foldChangeBySource ::
  (Ord source) =>
  UM.Change source () ->
  Map FilePath [source] ->
  Map FilePath [source]
foldChangeBySource ch m0 =
  Map.foldrWithKey
    ( \_tag files acc ->
        Map.foldrWithKey
          ( \fp act ->
              case act of
                UM.Refresh _ srcs ->
                  Map.insert fp (NE.toList $ fst <$> srcs)
                UM.Delete -> Map.delete fp
          )
          acc
          files
    )
    m0
    ch

-- | Brief delay to give fsnotify watchers time to come online before we
-- mutate files. fsnotify provides no synchronous readiness signal.
fsnotifyWatcherSetupDelay :: Int
fsnotifyWatcherSetupDelay = 200_000 -- 200ms

-- | Wait (up to the timeout) until the LVar matches the expected value.
-- Returns True if matched, False if we timed out.
waitForModel ::
  (MonadUnliftIO m, Eq a) =>
  LVar.LVar a ->
  a ->
  m Bool
waitForModel lv target = go modelWaitTimeout
  where
    pollInterval = 10_000 -- 10ms
    modelWaitTimeout = 5_000_000 -- 5s ceiling; fast path exits on first match
    go remaining = do
      v <- LVar.get lv
      if v == target
        then pure True
        else
          if remaining <= 0
            then pure False
            else do
              threadDelay pollInterval
              go (remaining - pollInterval)

-- | Represent the mutation of a folder over time.
--
-- Initial state of the folder, along with the mutations to perform, both as IO
-- actions.
data FolderMutation = FolderMutation
  { -- Mount point: the subfolder in which files must be shifted.
    _folderMountPoint :: Maybe FilePath,
    -- | How to initialize the folder
    _folderMutationInit :: IO (),
    -- | IO operations to perform for updating the folder
    _folderMutationUpdate :: IO ()
  }

runFolderMutation :: FolderMutation -> IO (Map.Map FilePath ByteString)
runFolderMutation folder = do
  withSystemTempDirectory "runFolderMutation" $ \tempDir -> do
    withCurrentDirectory tempDir $ do
      let withMountPointIfAny = case _folderMountPoint folder of
            Nothing -> id
            Just subdir -> \f -> do
              -- Create the mount point
              _ <- createDirectory subdir
              withCurrentDirectory subdir f
      withMountPointIfAny $ do
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
