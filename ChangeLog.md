
## Unreleased

- Add `unionMountStreaming`: a variant of `unionMount` whose handler is `Change source tag -> model -> m model` instead of `Change source tag -> m (model -> model)`. Two new capabilities fall out of the shape: (1) the handler can fold per-file updates through the running model so each file's closure is GC'd as soon as its update has been applied — important for downstream consumers that load very large directories on the initial mount; (2) the handler can read the current model when computing each step, removing the need for consumers to maintain a parallel mirror just to look at existing state inside the change handler. Existing `unionMount` is unchanged.
- Per-source ignore patterns for `unionMount`, replacing the prior flat `[FilePattern]` ignore list. The single `Map source [FilePattern]` parameter scopes ignores to the source they are keyed under, fixing the cross-source coupling where one layer's ignore list would silently hide matching files in sibling layers. To apply a pattern to every source ("universal" ignore), key it under every source — the per-source map can emulate the old global behavior, so the redundant flat list is gone. **Breaking change** for `unionMount` callers; `mount` is unaffected (it builds the singleton map internally). [#16](https://github.com/srid/unionmount/pull/16). Migration:

  ```diff
  -unionMount sources pats ignore         model0 handleAction
  +unionMount sources pats perSourceIgnore model0 handleAction
  ```

  - `perSourceIgnore = mempty` and dropping the old `ignore` argument preserves "no ignores at all".
  - To match the prior global semantics: `perSourceIgnore = Map.fromSet (const ignore) (Set.map fst sources)`.
  - To gain layer-scoped suppression: key each source's patterns separately.
- Broaden test coverage; expose pure internals as `System.UnionMount.Internal` (#15)

## 0.3.0.0

- Support for custom mount points (#6)

## 0.2.2.0

- Now requires fsnotify >= 0.4
  - Write a simple debouncer for files (fsnotify's debouncer was removed upstream). This is better than the removed upstream debouncer because it works like the proposed state machine. 

## 0.2.0.0

- Auto re-mount on unhandled folder events
- API refactor (No LVar) - [#1](https://github.com/srid/unionmount/pull/1)

## 0.1.0.0

- Initial release
