This library provides Haskell bindings to
[LevelDB](https://github.com/google/leveldb)

[![Build Status](https://secure.travis-ci.org/kim/leveldb-haskell.png)](http://travis-ci.org/kim/leveldb-haskell)

## Installation

Prerequisites:

* [GHC 7.*](http://www.haskell.org/ghc)
* [Cabal](http://www.haskell.org/cabal), version 1.3 or higher
* [LevelDB](https://github.com/google/leveldb)
* Optional: [Snappy](http://code.google.com/p/snappy),
  if compression support is desired

**Note:** as of version 1.3, LevelDB can be built as a shared library. Thus, as
of version 0.1.0 of this library, LevelDB is no longer bundled and must be
installed on the target system. On many systems / distributions, the LevelDB
library is available via the native package manager. On Windows, you may want to
follow [these instructions](https://github.com/lamdu/lamdu/blob/1623bc38e67361d4ba4f051e23a66985a66ca52c/doc/Build.md#windows).

To install the latest version from hackage:

```shell
$ cabal install leveldb-haskell
```

To install from checked-out source:

```shell
$ cabal install
```

## Notes

This library is in very early stage and has seen very limited testing. Comments
and contributions are welcome.

## Bugs and Contributing

Please report issues via http://github.com/kim/leveldb-haskell/issues.<br />
Patches are best submitted as pull requests, or via email
(kim.altintop@gmail.com).

## License

BSD 3, see LICENSE file.
