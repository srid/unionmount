{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}

module System.UnionMount
  ( -- * Mount endpoints
    mount,
    unionMount,
    unionMount',

    -- * Types
    FileAction (..),
    RefreshAction (..),
    Change,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelWarn),
    MonadLogger,
    logWithoutLoc,
  )
import Data.LVar qualified as LVar
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Directory (canonicalizePath)
import System.FSNotify
  ( ActionPredicate,
    Event (..),
    EventIsDirectory (IsDirectory),
    StopListening,
    WatchManager,
    defaultConfig,
    eventIsDirectory,
    eventPath,
    watchTree,
    withManagerConf,
  )
import System.FilePath (isRelative, makeRelative)
import System.FilePattern (FilePattern, (?==))
import System.FilePattern.Directory (getDirectoryFilesIgnore)
import UnliftIO (MonadUnliftIO, finally, newTBQueueIO, race, try, withRunInIO, writeTBQueue)
import UnliftIO.STM (TBQueue, readTBQueue)

-- | Simplified version of `unionMount` with exactly one layer.
mount ::
  forall model m b.
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLogger m,
    Show b,
    Ord b
  ) =>
  -- | The directory to mount.
  FilePath ->
  -- | Only include these files (exclude everything else)
  [(b, FilePattern)] ->
  -- | Ignore these patterns
  [FilePattern] ->
  -- | Initial value of model, onto which to apply updates.
  model ->
  -- | How to update the model given a file action.
  --
  -- `b` is the tag associated with the `FilePattern` that selected this
  -- `FilePath`. `FileAction` is the operation performed on this path. This
  -- should return a function (in monadic context) that will update the model,
  -- to reflect the given `FileAction`.
  --
  -- If the action throws an exception, it will be logged and ignored.
  (b -> FilePath -> FileAction () -> m (model -> model)) ->
  m (model, (model -> m ()) -> m ())
mount folder pats ignore var0 toAction' =
  let tag0 = ()
      sources = one (tag0, folder)
   in unionMount sources pats ignore var0 $ \ch -> do
        let fsSet = (fmap . fmap . fmap . fmap) void $ fmap Map.toList <$> Map.toList ch
        (\(tag, xs) -> uncurry (toAction' tag) `chainM` xs) `chainM` fsSet
  where
    -- Monadic version of `chain`
    chainM :: (Monad m) => (x -> m (a -> a)) -> [x] -> m (a -> a)
    chainM f =
      fmap chain . mapM f
      where
        -- Apply the list of actions in the given order to an initial argument.
        --
        -- chain [f1, f2, ...] a = ... (f2 (f1 x))
        chain :: [a -> a] -> a -> a
        chain = flip $ foldl' $ flip ($)

-- | Union mount a set of sources (directories) into a model.
unionMount ::
  forall source tag model m.
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLogger m,
    Ord source,
    Ord tag
  ) =>
  Set (source, FilePath) ->
  [(tag, FilePattern)] ->
  [FilePattern] ->
  model ->
  (Change source tag -> m (model -> model)) ->
  m (model, (model -> m ()) -> m ())
unionMount sources pats ignore model0 handleAction = do
  (x0, xf) <- unionMount' sources pats ignore
  x0' <- interceptExceptions id $ handleAction x0
  let initial = x0' model0
  lvar <- LVar.new initial
  let sender send = do
        Cmd_Remount <- xf $ \change -> do
          change' <- interceptExceptions id $ handleAction change
          LVar.modify lvar change'
          x <- LVar.get lvar
          send x
        log LevelInfo "Remounting..."
        (a, b) <- unionMount sources pats ignore model0 handleAction
        send a
        b send
  pure (x0' model0, sender)

-- Log and ignore exceptions
--
-- TODO: Make user define-able?
interceptExceptions :: (MonadIO m, MonadUnliftIO m, MonadLogger m) => a -> m a -> m a
interceptExceptions default_ f = do
  try f >>= \case
    Left (ex :: SomeException) -> do
      log LevelError $ "Change handler exception: " <> show ex
      pure default_
    Right v ->
      pure v

-------------------------------------
-- Candidate for moving to a library
-------------------------------------

data Evt source tag
  = Evt_Change (Change source tag)
  | Evt_Unhandled
  deriving (Eq, Show)

data Cmd
  = Cmd_Remount
  deriving (Eq, Show)

-- | Like `unionMount` but without exception interrupting or re-mounting.
unionMount' ::
  forall source tag m m1.
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLogger m,
    MonadLogger m1,
    MonadIO m1,
    Ord source,
    Ord tag
  ) =>
  Set (source, FilePath) ->
  [(tag, FilePattern)] ->
  [FilePattern] ->
  m1
    ( Change source tag,
      (Change source tag -> m ()) ->
      m Cmd
    )
unionMount' sources pats ignore = do
  fmap fst . flip runStateT (emptyOverlayFs @source) $ do
    -- Initial traversal of sources
    changes0 :: Change source tag <-
      fmap snd . flip runStateT Map.empty $ do
        forM_ sources $ \(src, folder) -> do
          taggedFiles <- filesMatchingWithTag folder pats ignore
          forM_ taggedFiles $ \(tag, fs) -> do
            forM_ fs $ \fp -> do
              put =<< lift . changeInsert src tag fp (Refresh Existing ()) =<< get
    ofs <- get
    pure
      ( changes0,
        \reportChange -> do
          -- Run fsnotify on sources
          q :: TBQueue (x, FilePath, Either (FolderAction ()) (FileAction ())) <- liftIO $ newTBQueueIO 1
          fmap (either id id) $
            race (onChange q (toList sources)) $
              fmap fst . flip runStateT ofs $ do
                let loop = do
                      (src, fp, actE) <- atomically $ readTBQueue q
                      let shouldIgnore = any (?== fp) ignore
                      case actE of
                        Left _ -> do
                          let reason = "Unhandled folder event on '" <> toText fp <> "'"
                          if shouldIgnore
                            then do
                              log LevelWarn $ reason <> " on an ignored path"
                              loop
                            else do
                              -- We don't know yet how to deal with folder events. Just reboot the mount.
                              log LevelWarn $ reason <> "; suggesting a re-mount"
                              pure Cmd_Remount -- Exit, asking user to remokunt
                        Right act -> do
                          case guard (not shouldIgnore) >> getTag pats fp of
                            Nothing -> loop
                            Just tag -> do
                              changes <- fmap snd . flip runStateT Map.empty $ do
                                put =<< lift . changeInsert src tag fp act =<< get
                              lift $ reportChange changes
                              loop
                loop
      )

filesMatching :: (MonadIO m, MonadLogger m) => FilePath -> [FilePattern] -> [FilePattern] -> m [FilePath]
filesMatching parent' pats ignore = do
  parent <- liftIO $ canonicalizePath parent'
  log LevelInfo $ toText $ "Traversing " <> parent <> " for files matching " <> show pats <> ", ignoring " <> show ignore
  liftIO $ getDirectoryFilesIgnore parent pats ignore

-- | Like `filesMatching` but with a tag associated with a pattern so as to be
-- able to tell which pattern a resulting filepath is associated with.
filesMatchingWithTag :: (MonadIO m, MonadLogger m, Ord b) => FilePath -> [(b, FilePattern)] -> [FilePattern] -> m [(b, [FilePath])]
filesMatchingWithTag parent' pats ignore = do
  fs <- filesMatching parent' (snd <$> pats) ignore
  let m = Map.fromListWith (<>) $
        flip mapMaybe fs $ \fp -> do
          tag <- getTag pats fp
          pure (tag, one fp)
  pure $ Map.toList m

getTag :: [(b, FilePattern)] -> FilePath -> Maybe b
getTag pats fp =
  let pull patterns =
        listToMaybe $
          flip mapMaybe patterns $ \(tag, pat) -> do
            guard $ pat ?== fp
            pure tag
   in if isRelative fp
        then pull pats
        else -- `fp` is an absolute path (because of use of symlinks), so let's
        -- be more lenient in matching it. Note that this does meat we might
        -- match files the user may not have originally intended. This is
        -- the trade offs with using symlinks.
          pull $ second ("**/" <>) <$> pats

data RefreshAction
  = -- | No recent change. Just notifying of file's existance
    Existing
  | -- | New file got created
    New
  | -- | The already existing file was updated.
    Update
  deriving (Eq, Show)

data FileAction a
  = -- | A new file, or updated file, is available
    Refresh RefreshAction a
  | -- | The file just got deleted.
    Delete
  deriving (Eq, Show, Functor)

-- | This is not an action on file, rather an action on a directory (which
-- may contain files, which would be outside the scope of this fsnotify event,
-- and so the user must manually deal with them.)
newtype FolderAction a = FolderAction a
  deriving (Eq, Show, Functor)

refreshAction :: FileAction a -> Maybe RefreshAction
refreshAction = \case
  Refresh act _ -> Just act
  _ -> Nothing

onChange ::
  forall x m.
  (MonadIO m, MonadLogger m, MonadUnliftIO m) =>
  TBQueue (x, FilePath, Either (FolderAction ()) (FileAction ())) ->
  [(x, FilePath)] ->
  -- | The filepath is relative to the folder being monitored, unless if its
  -- ancestor is a symlink.
  m Cmd
onChange q roots = do
  withManagerM $ \mgr -> do
    stops <- forM roots $ \(x, rootRel) -> do
      -- NOTE: It is important to use canonical path, because this will allow us to
      -- transform fsnotify event's (absolute) path into one that is relative to
      -- @parent'@ (as passed by user), which is what @f@ will expect.
      root <- liftIO $ canonicalizePath rootRel
      log LevelInfo $ toText $ "Monitoring " <> root <> " for changes"
      watchTreeM mgr root (const True) $ \event -> do
        log LevelDebug $ show event
        let fp = makeRelative root $ eventPath event
            f act = atomically $ writeTBQueue q (x, fp, act)
        if eventIsDirectory event == IsDirectory
          then f $ Left $ FolderAction ()
          else case event of
            Added {} -> f $ Right $ Refresh New ()
            Modified {} -> f $ Right $ Refresh Update ()
            ModifiedAttributes {} -> f $ Right $ Refresh Update ()
            Removed {} -> f $ Right Delete
            _ -> pure ()
    liftIO (threadDelay maxBound)
      `finally` do
        log LevelInfo "Stopping fsnotify monitor."
        liftIO $ forM_ stops id
    -- Unreachable
    pure Cmd_Remount

withManagerM ::
  (MonadIO m, MonadUnliftIO m) =>
  (WatchManager -> m a) ->
  m a
withManagerM f = do
  withRunInIO $ \run ->
    withManagerConf defaultConfig $ \mgr -> run (f mgr)

watchTreeM ::
  forall m.
  (MonadIO m, MonadUnliftIO m) =>
  WatchManager ->
  FilePath ->
  ActionPredicate ->
  (Event -> m ()) ->
  m StopListening
watchTreeM wm fp pr f =
  withRunInIO $ \run ->
    watchTree wm fp pr $ \evt -> run (f evt)

log :: MonadLogger m => LogLevel -> Text -> m ()
log = logWithoutLoc "System.UnionMount"

-- TODO: Abstract in module with StateT / MonadState
newtype OverlayFs source = OverlayFs (Map FilePath (Set source))

-- TODO: Replace this with a function taking `NonEmpty source`
emptyOverlayFs :: Ord source => OverlayFs source
emptyOverlayFs = OverlayFs mempty

overlayFsModify :: FilePath -> (Set src -> Set src) -> OverlayFs src -> OverlayFs src
overlayFsModify k f (OverlayFs m) =
  OverlayFs $
    Map.insert k (f $ fromMaybe Set.empty $ Map.lookup k m) m

overlayFsAdd :: Ord src => FilePath -> src -> OverlayFs src -> OverlayFs src
overlayFsAdd fp src =
  overlayFsModify fp $ Set.insert src

overlayFsRemove :: Ord src => FilePath -> src -> OverlayFs src -> OverlayFs src
overlayFsRemove fp src =
  overlayFsModify fp $ Set.delete src

overlayFsLookup :: FilePath -> OverlayFs source -> Maybe (NonEmpty (source, FilePath))
overlayFsLookup fp (OverlayFs m) = do
  sources <- nonEmpty . toList =<< Map.lookup fp m
  pure $ sources <&> (,fp)

-- Files matched by each tag pattern, each represented by their corresponding
-- file (absolute path) in the individual sources. It is up to the user to union
-- them (for now).
type Change source tag = Map tag (Map FilePath (FileAction (NonEmpty (source, FilePath))))

-- | Report a change to overlay fs
changeInsert ::
  (Ord source, Ord tag, MonadState (OverlayFs source) m) =>
  source ->
  tag ->
  FilePath ->
  FileAction () ->
  Change source tag ->
  m (Change source tag)
changeInsert src tag fp act ch = do
  fmap snd . flip runStateT ch $ do
    -- First, register this change in the overlayFs
    lift $
      modify $
        (if act == Delete then overlayFsRemove else overlayFsAdd)
          fp
          src
    overlays <-
      lift (gets $ overlayFsLookup fp) <&> \case
        Nothing -> Delete
        Just fs ->
          -- We don't track per-source action (not ideal), so use 'Existing'
          -- only if the current action is 'Deleted'. In every other scenario,
          -- re-use the current action for all overlay files.
          let combinedAction = fromMaybe Existing $ refreshAction act
           in Refresh combinedAction fs
    gets (Map.lookup tag) >>= \case
      Nothing ->
        modify $ Map.insert tag $ Map.singleton fp overlays
      Just files ->
        modify $ Map.insert tag $ Map.insert fp overlays files
