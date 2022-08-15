# unionmount

Haskell library to "[union mount](https://en.wikipedia.org/wiki/Union_mount)" a bunch of folders onto an in-memory data structure, and keeping the latter in sync as the files change over time. Used in [Ema](https://ema.srid.ca) and [Emanote](https://emanote.srid.ca).

See [this example](https://github.com/srid/ema-template/blob/c2f1cc491aae7342ec783c87a61fbe0a73754906/src/Main.hs#L205-L213) illustrating mounting a directory of Markdown files into (effectively) a `Map FilePath String`. A [more involved example](https://github.com/srid/emanote/blob/f35d4a14cd5dfa2a871f926d8537e56908806da8/src/Emanote/Source.hs#L28-L34) from Emanote demonstrates the "union" aspect of the library.
