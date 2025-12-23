module System.UnionMount.Ignore
  ( readIgnoreFile,
  )
where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.FilePath ((</>))
import System.FilePattern (FilePattern)
import System.FilePattern.Directory (getDirectoryFilesIgnore)

-- | Read ignore patterns from the specified ignore file in all sources.
--
-- If multiple sources contain the ignore file, patterns from all of them are
-- combined (union). If the ignore file is not found in a source, it is silently
-- skipped. Comments (lines starting with #) and empty lines are ignored.
readIgnoreFile ::
  (MonadIO m) =>
  -- | Name of the ignore file (e.g., ".emanoteignore")
  FilePath ->
  -- | List of source directories to search for the ignore file
  [FilePath] ->
  m [FilePattern]
readIgnoreFile file folders = liftIO $ do
  mconcat
    <$> traverse
      ( \folder -> do
          files <- getDirectoryFilesIgnore folder [file] []
          mconcat
            <$> traverse
              ( \f -> do
                  s <- BS.readFile $ folder </> f
                  pure $ parseIgnorePatterns $ TE.decodeUtf8 s
              )
              files
      )
      folders
  where
    parseIgnorePatterns :: Text -> [FilePattern]
    parseIgnorePatterns =
      fmap (toString . T.strip) . filter (not . T.isPrefixOf "#") . filter (not . T.null) . T.lines
