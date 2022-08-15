# unionmount

Haskell library to "[union mount](https://en.wikipedia.org/wiki/Union_mount)" a bunch of folders onto an in-memory data structure, and keeping the latter in sync as the files change over time. Used in [Ema](https://ema.srid.ca) and [Emanote](https://emanote.srid.ca).

## Usage

Both the `mount` and `unionMount` functions return a tuple value of type [Dynamic](https://ema.srid.ca/guide/model/dynamic), giving direct access to the initial value as well as the updater function that may be run in a separate thread. See [how Ema uses it](https://github.com/EmaApps/ema/blob/459d3899e0b9ea13e23c81126279dc62530b994c/src/Ema/App.hs#L72-L84) for an illustration.

### Examples

See [this example](https://github.com/EmaApps/ema/blob/459d3899e0b9ea13e23c81126279dc62530b994c/src/Ema/Route/Lib/Extra/PandocRoute.hs#L132-L139) illustrating mounting a directory of Markdown files into (effectively) a `Map FilePath String`. A [more involved example](https://github.com/EmaApps/emanote/blob/7c49c73cd3b7dbeace72353574f3decfb68929f2/src/Emanote/Source/Dynamic.hs#L58-L64) from Emanote demonstrates the "union" aspect of the library.
