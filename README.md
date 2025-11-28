# unionmount

Haskell library to "[union mount](https://en.wikipedia.org/wiki/Union_mount)" a bunch of folders onto an in-memory data structure, and keeping the latter in sync as the files change over time. Used in [Ema](https://ema.srid.ca) and [Emanote](https://emanote.srid.ca).

## Usage

Both the `mount` and `unionMount` functions return a tuple value of type [Dynamic](https://ema.srid.ca/guide/model/dynamic), giving direct access to the initial value as well as the updater function that may be run in a separate thread. See [how Ema uses it](https://github.com/EmaApps/ema/blob/459d3899e0b9ea13e23c81126279dc62530b994c/src/Ema/App.hs#L72-L84) for an illustration.

Here's a simple example of loading Markdown files onto a TVar of `Map FilePath Text` (file contents keyed by path).

```haskell
import System.UnionMount qualified as UM
import Data.Map.Strict qualified as Map
import Colog.Core (logTextStdout, filterBySeverity, Severity (..))

main :: IO ()
main = do
  let logger = filterBySeverity Info Severity logTextStdout  -- Log Info and above
      baseDir = "/Users/srid/Documents/Notebook"
  (model0, modelF) <- UM.mount logger baseDir (one ((), "*.md")) [] mempty (const $ handlePathUpdate baseDir)
  modelVar <- newTVarIO model0
  modelF $ \newModel -> do
    atomically $ writeTVar modelVar newModel

handlePathUpdate ::
  (MonadIO m) =>
  FilePath -> FilePath -> UM.FileAction () -> m (Map FilePath Text -> Map FilePath Text)
handlePathUpdate baseDir path action = do
  case action of
    UM.Refresh _ _ -> do
      s <- decodeUtf8 <$> readFileBS (baseDir </> path)
      pure $ Map.insert path s
    UM.Delete -> do
      pure $ Map.delete path
```

### Logging

unionmount uses [co-log-core](https://hackage.haskell.org/package/co-log-core) which has zero dependencies and allows you to bring your own logging implementation. The logger type is `LogAction m (WithSeverity Text)`, supporting Debug, Info, Warning, and Error severity levels.

- **No logging**: Pass `mempty` as the logger
- **co-log**: Use any co-log logger (with severity filtering)
- **monad-logger**: Create a LogAction that calls your monad-logger functions
- **Custom**: Implement your own `LogAction m (WithSeverity Text)`

```haskell
-- No logging
UM.mount mempty folder pats ignore model handleAction

-- Using co-log with severity filtering
import Colog.Core (logTextStdout, filterBySeverity, Severity (..))
let logger = filterBySeverity Info Severity logTextStdout  -- Only Info and above
UM.mount logger folder pats ignore model handleAction
```

### Examples

See [this example](https://github.com/EmaApps/ema/blob/459d3899e0b9ea13e23c81126279dc62530b994c/src/Ema/Route/Lib/Extra/PandocRoute.hs#L132-L139) illustrating mounting a directory of Markdown files into (effectively) a `Map FilePath String`. A [more involved example](https://github.com/EmaApps/emanote/blob/7c49c73cd3b7dbeace72353574f3decfb68929f2/src/Emanote/Source/Dynamic.hs#L58-L64) from Emanote demonstrates the "union" aspect of the library.
