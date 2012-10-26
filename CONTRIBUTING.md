# Contributing

## Hacker notes

### Updating LevelDB source code

To update the LevelDB source code, update the `GITVERSION`
variable inside `makefile`, and make it point to whatever
commit you want to update to. You can also change `GITURL`
to another repository.

Then run `make`, which will copy out all the new source code
into `src/cbits/leveldb` and delete the git checkout.

### Finding the needed C files

To find the files that should be included in the Haskell library via
`c-sources` in the cabal file, run:

    $ ack -a -f --cc src/cbits | grep -v test | grep -v "\.h\$" | grep "\.cc\$" | sort
