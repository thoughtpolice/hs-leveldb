# Haskell bindings for LevelDB

[LevelDB][] is a fast key-value storage library by Google.

These are the awesome Haskell bindings.

[travis-ci.org](http://travis-ci.org) results: [![Build Status](https://secure.travis-ci.org/thoughtpolice/hs-leveldb.png?branch=master)](http://travis-ci.org/thoughtpolice/hs-leveldb)

[Homepage][main page].

# Features

Most of the features of the C bindings:

  * Automatic compression if you have [Snappy](http://snappy.googlecode.com) installed.
  * Snapshots.
  * Repair functionality.
  * Batch-based atomic writes.
  * Iterator support for enumerating key ranges.
  * Filter policies allow you to trade memory for disk seeks.
  * Database/key-range compaction.
  * Properties and approximate sizes on the filesystem.
  * Tiered bindings, offering high level, mid-level and low-level interfaces.

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install leveldb
```

To install the latest git version, you'll need `autoconf` so
you can regenerate `./configure`:

```bash
$ autoconf
$ cabal install
```

# Join in

File bugs in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/hs-leveldb.git`

There's also a [BitBucket mirror][bb]:

* `git clone https://bitbucket.org/thoughtpolice/hs-leveldb.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/hs-leveldb/master/AUTHORS.txt).

# License

BSD3. See `LICENSE.txt` for terms of copyright and redistribution.

[LevelDB]: http://code.google.com/p/leveldb/
[main page]: http://thoughtpolice.github.com/hs-leveldb
[issue tracker]: http://github.com/thoughtpolice/hs-leveldb/issues
[gh]: http://github.com/thoughtpolice/hs-leveldb
[bb]: http://bitbucket.org/thoughtpolice/hs-leveldb
[Hackage]: http://hackage.haskell.org/package/leveldb
