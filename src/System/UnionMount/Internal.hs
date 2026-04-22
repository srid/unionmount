-- | Internal helpers used by "System.UnionMount" and its test suite.
--
-- These are exposed as an internal module rather than re-exported from
-- "System.UnionMount" so that they are discoverable to tests without
-- polluting the public API. Stability is not guaranteed across versions.
module System.UnionMount.Internal
  ( -- * Tag resolution
    getTag,

    -- * Overlay filesystem
    OverlayFs,
    emptyOverlayFs,
    overlayFsModify,
    overlayFsAdd,
    overlayFsRemove,
    overlayFsLookup,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.FilePath (isRelative)
import System.FilePattern (FilePattern, (?==))

-- | Resolve the first tag whose pattern matches @fp@.
--
-- If @fp@ is an absolute path (which can happen when the mount directory
-- contains symlinks), patterns are implicitly prefixed with @**/@ so that
-- the tail of the path can still match.
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
        -- be more lenient in matching it. Note that this does mean we might
        -- match files the user may not have originally intended. This is
        -- the trade off with using symlinks.
          pull $ second ("**/" <>) <$> pats

-- TODO: Abstract in module with StateT / MonadState
newtype OverlayFs source = OverlayFs (Map FilePath (Set (source, FilePath)))

-- TODO: Replace this with a function taking `NonEmpty source`
emptyOverlayFs :: (Ord source) => OverlayFs source
emptyOverlayFs = OverlayFs mempty

overlayFsModify :: FilePath -> (Set (src, FilePath) -> Set (src, FilePath)) -> OverlayFs src -> OverlayFs src
overlayFsModify k f (OverlayFs m) =
  OverlayFs $
    Map.insert k (f $ fromMaybe Set.empty $ Map.lookup k m) m

overlayFsAdd :: (Ord src) => FilePath -> (src, FilePath) -> OverlayFs src -> OverlayFs src
overlayFsAdd fp src =
  overlayFsModify fp $ Set.insert src

overlayFsRemove :: (Ord src) => FilePath -> (src, FilePath) -> OverlayFs src -> OverlayFs src
overlayFsRemove fp src =
  overlayFsModify fp $ Set.delete src

overlayFsLookup :: FilePath -> OverlayFs source -> Maybe (NonEmpty ((source, FilePath), FilePath))
overlayFsLookup fp (OverlayFs m) = do
  sources <- nonEmpty . toList =<< Map.lookup fp m
  pure $ sources <&> (,fp)
