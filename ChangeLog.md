
## Unreleased (0.3.0.0)

- Support for custom mount points (#6)

## 0.2.2.0

- Now requires fsnotify >= 0.4
  - Write a simple debouncer for files (fsnotify's debouncer was removed upstream). This is better than the removed upstream debouncer because it works like the proposed state machine. 

## 0.2.0.0

- Auto re-mount on unhandled folder events
- API refactor (No LVar) - [#1](https://github.com/srid/unionmount/pull/1)

## 0.1.0.0

- Initial release
