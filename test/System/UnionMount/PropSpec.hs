module System.UnionMount.PropSpec where

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import System.UnionMountSpec (FolderMutation (..), unionMountSpec)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
import UnliftIO.Directory (doesFileExist, removeFile)

spec :: Spec
spec = describe "unionMount (property)" $ do
  -- fsnotify-based tests are expensive (~0.5s each), so we cap the
  -- number of cases. This is enough to catch regressions in the
  -- model-vs-IO equivalence invariant without ballooning CI time.
  modifyMaxSuccess (const 15) $
    prop "final model equals the IO-materialized state" $
      \(UnionFolderMutationSpec specs) ->
        unionMountSpec (toFolderMutation <$> specs)

-- | Pure description of a file-system operation in a folder.
data FileOp
  = WriteF FilePath ByteString
  | RemoveF FilePath
  deriving (Show, Eq)

-- | Pure description of a `FolderMutation`.
data FolderMutationSpec = FolderMutationSpec
  { fmsMountPoint :: Maybe FilePath,
    fmsInitOps :: [FileOp],
    fmsUpdateOps :: [FileOp]
  }
  deriving (Show, Eq)

-- | Non-empty list of folder mutation specs (one per layer).
newtype UnionFolderMutationSpec = UnionFolderMutationSpec (NonEmpty FolderMutationSpec)
  deriving (Show)

instance Arbitrary FolderMutationSpec where
  arbitrary = do
    mp <- oneof [pure Nothing, Just <$> elements ["foo", "bar"]]
    (initOps, afterInit) <- genOps mempty
    (updateOps, _) <- genOps afterInit
    pure $ FolderMutationSpec mp initOps updateOps

instance Arbitrary UnionFolderMutationSpec where
  arbitrary = do
    k <- choose (1, 3)
    UnionFolderMutationSpec . NE.fromList <$> vectorOf k arbitrary

-- | Generate a sequence of ops. `RemoveF` is only emitted when the target
-- file has been written by an earlier op in the same sequence (or is in
-- the initial set), so the generated IO is safe to run.
genOps :: Set FilePath -> Gen ([FileOp], Set FilePath)
genOps = go (4 :: Int)
  where
    go 0 present = pure ([], present)
    go k present = do
      op <-
        if Set.null present
          then genWrite
          else frequency [(3, genWrite), (1, RemoveF <$> elements (Set.toList present))]
      let present' = case op of
            WriteF fp _ -> Set.insert fp present
            RemoveF fp -> Set.delete fp present
      (rest, final) <- go (k - 1) present'
      pure (op : rest, final)
    genWrite = WriteF <$> elements ["a", "b", "c", "d"] <*> genContent
    genContent = BS.pack <$> listOf (elements [0x41 .. 0x5A])

runFileOp :: FileOp -> IO ()
runFileOp = \case
  WriteF fp bs -> BS.writeFile fp bs
  RemoveF fp -> whenM (doesFileExist fp) $ removeFile fp

toFolderMutation :: FolderMutationSpec -> FolderMutation
toFolderMutation s =
  FolderMutation
    { _folderMountPoint = fmsMountPoint s,
      _folderMutationInit = traverse_ runFileOp (fmsInitOps s),
      _folderMutationUpdate = traverse_ runFileOp (fmsUpdateOps s)
    }
