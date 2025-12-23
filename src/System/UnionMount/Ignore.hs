module System.UnionMount.Ignore
  ( IgnorePattern (..),
    IgnoreConfig (..),
    readIgnoreFile,
    applyIgnorePatterns,
    readIgnoreFileWithGlobalPats,
    applyIgnoreConfigToSources,
  )
where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.FilePattern (FilePattern)
import System.FilePattern.Directory (getDirectoryFilesIgnore)

-- | Represents an ignore pattern from an ignore file.
--
-- Patterns starting with @!@ are negations that remove patterns from the ignore list.
data IgnorePattern
  = -- | Ignore files matching this pattern
    Ignore FilePattern
  | -- | Un-ignore files matching this pattern (removes from ignore list)
    UnIgnore FilePattern
  deriving (Eq, Show)

-- | Configuration for ignore file behavior.
data IgnoreConfig = IgnoreConfig
  { -- | Global ignore patterns to apply to all sources
    globalPatterns :: [FilePattern],
    -- | Optional ignore file name (e.g., @.emanoteignore@)
    ignoreFileName :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | Read and parse ignore patterns from an ignore file.
--
-- Patterns starting with @!@ are parsed as 'UnIgnore', all others as 'Ignore'.
-- Comments (lines starting with @#@) and empty lines are filtered out.
--
-- If the ignore file doesn't exist, returns an empty list.
readIgnoreFile ::
  (MonadIO m) =>
  -- | Path to ignore file (e.g., @\/path\/to\/folder\/.emanoteignore@)
  FilePath ->
  -- | Parsed ignore patterns
  m [IgnorePattern]
readIgnoreFile ignoreFilePath = liftIO $ do
  let folder = takeDirectory ignoreFilePath
      file = takeFileName ignoreFilePath

  files <- getDirectoryFilesIgnore folder [file] []
  case viaNonEmpty head files of
    Nothing -> pure []
    Just f -> do
      content <- BS.readFile (folder </> f)
      pure $ parseIgnoreFile (TE.decodeUtf8 content)
  where
    parseIgnoreFile :: Text -> [IgnorePattern]
    parseIgnoreFile =
      fmap toIgnorePattern
        . fmap T.strip
        . filter (not . T.isPrefixOf "#")
        . filter (not . T.null)
        . T.lines

    toIgnorePattern :: Text -> IgnorePattern
    toIgnorePattern pat
      | T.isPrefixOf "!" pat = UnIgnore (toString $ T.drop 1 pat)
      | otherwise = Ignore (toString pat)

-- | Apply ignore patterns to a base set of global patterns.
--
-- Processes patterns in order:
--
-- - 'Ignore' patterns are appended to the list
-- - 'UnIgnore' patterns remove matching patterns from the list
--
-- Example:
--
-- >>> applyIgnorePatterns [".*"] [Ignore "*.tmp"]
-- [".*", "*.tmp"]
--
-- >>> applyIgnorePatterns [".*"] [UnIgnore ".*", Ignore "*.tmp"]
-- ["*.tmp"]
applyIgnorePatterns ::
  -- | Global patterns to start with
  [FilePattern] ->
  -- | Local patterns to apply
  [IgnorePattern] ->
  -- | Effective patterns
  [FilePattern]
applyIgnorePatterns = foldl' applyPattern
  where
    applyPattern :: [FilePattern] -> IgnorePattern -> [FilePattern]
    applyPattern acc (Ignore pat) = acc ++ [pat]
    applyPattern acc (UnIgnore pat) = filter (/= pat) acc

-- | Read an ignore file and apply its patterns to global patterns.
--
-- This is a convenience function that combines 'readIgnoreFile' and 'applyIgnorePatterns'.
--
-- Example:
--
-- >>> readIgnoreFileWithGlobalPats [".*"] "/path/to/folder/.emanoteignore"
-- -- Returns global patterns with local modifications applied
readIgnoreFileWithGlobalPats ::
  (MonadIO m) =>
  -- | Global patterns to start with
  [FilePattern] ->
  -- | Path to ignore file
  FilePath ->
  -- | Effective patterns
  m [FilePattern]
readIgnoreFileWithGlobalPats globalPats ignoreFilePath = do
  localPats <- readIgnoreFile ignoreFilePath
  pure $ applyIgnorePatterns globalPats localPats

-- | Apply ignore configuration to multiple sources.
--
-- For each source, if an ignore file name is specified in the config, reads that
-- file from the source's folder and applies the patterns. Otherwise, returns the
-- global patterns for all sources.
--
-- Returns a Map from source to effective ignore patterns for that source.
applyIgnoreConfigToSources ::
  (MonadIO m, Ord s) =>
  IgnoreConfig ->
  [(s, FilePath)] ->
  m (Map s [FilePattern])
applyIgnoreConfigToSources (IgnoreConfig globalPats maybeIgnoreFile) sources = do
  case maybeIgnoreFile of
    Nothing -> pure $ Map.fromList [(src, globalPats) | (src, _) <- sources]
    Just ignoreFile -> do
      fmap Map.fromList $ forM sources $ \(src, folder) -> do
        effectivePats <- readIgnoreFileWithGlobalPats globalPats (folder </> ignoreFile)
        pure (src, effectivePats)
