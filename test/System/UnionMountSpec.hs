module System.UnionMountSpec where

import Control.Monad.Logger.Extras (logToStderr, runLoggerLoggingT)
import System.FilePath ((</>))
import System.FilePattern (FilePattern)
import System.UnionMount qualified as UM
import Test.Hspec
import UnliftIO.Async (race_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM (writeTMVar)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "unionmount" $ do
    it "single file update works" $ do
      -- Create a temporary directory, add a file to it, call `mount`, make an update to that file, and check that it is updated in memory.
      withSystemTempDirectory "unionmount" $ \tempDir -> do
        writeFile (tempDir </> "file1") "hello"
        model <- newEmptyTMVarIO
        flip runLoggerLoggingT logToStderr $ do
          (model0, update) <- UM.mount tempDir allFiles ignoreNone mempty (const accumulateActions)
          atomically $ writeTMVar model model0
          race_
            (update $ atomically . writeTMVar model)
            ( do
                threadDelay 1000000 -- Wait for the initial model to be loaded.
                writeFile (tempDir </> "file1") "hello, again"
                threadDelay 1000000 -- Wait for fsnotify to handle events
            )
        finalModel <- atomically $ readTMVar model
        finalModel
          `shouldBe` [ ("file1", UM.Refresh UM.Existing ()),
                       ("file1", UM.Refresh UM.Update ())
                     ]

allFiles :: [((), FilePattern)]
allFiles = [((), "*")]

ignoreNone :: [a]
ignoreNone = []

accumulateActions :: (Applicative f, Show a, Show b) => a -> b -> f ([(a, b)] -> [(a, b)])
accumulateActions fp act = pure $ reverse . (:) (fp, act)
