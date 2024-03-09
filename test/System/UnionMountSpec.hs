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
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "unionmount" $ do
    it "single file update works" $ do
      -- Create a temporary directory, add a file to it, call `mount`, make an update to that file, and check that it is updated in memory.
      withSystemTempDirectory "unionmount" $ \tempDir -> do
        writeFile (tempDir </> "file1") "hello"
        model <- LVar.empty
        flip runLoggerLoggingT logToStderr $ do
          (model0, update) <- UM.mount tempDir allFiles ignoreNone mempty $ \() fp -> \case
            UM.Delete -> pure $ Map.delete fp
            UM.Refresh _ () -> do
              s <- readFileBS $ tempDir </> fp
              pure $ Map.insert fp s
          LVar.set model model0
          race_
            (update $ LVar.set model)
            ( do
                threadDelay 1000000 -- Wait for the initial model to be loaded.
                writeFile (tempDir </> "file1") "hello, again"
                writeFile (tempDir </> "file2") "another file"
                threadDelay 1000000 -- Wait for fsnotify to handle events
            )
        finalModel <- LVar.get model
        finalModel
          `shouldBe` Map.fromList
            [ ("file1", "hello, again"),
              ("file2", "another file")
            ]

allFiles :: [((), FilePattern)]
allFiles = [((), "*")]

ignoreNone :: [a]
ignoreNone = []
