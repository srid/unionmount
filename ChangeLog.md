
## Unreleased

- Add per-source ignore patterns to `unionMount`. The new `Map source [FilePattern]` parameter scopes ignores to a single source, fixing the cross-source coupling where one layer's ignore list would silently hide matching files in other layers. **Breaking change** for `unionMount` callers; `mount` is unaffected. Migration:

  ```diff
  -unionMount sources pats ignore         model0 handleAction
  +unionMount sources pats ignore mempty  model0 handleAction
  ```

  Pass `mempty` for the new parameter to preserve prior behavior. Move any layer-specific patterns out of the global list and into `Map.singleton <source> [<patterns>]` to gain layer-scoped suppression.
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
