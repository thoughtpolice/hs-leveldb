Extracted from LevelDB 1.5.0 (commit dd0d562b4d4fbd07db6a44f9e221f8d368fee8e4)

To find the files that should be included in the Haskell library via
`c-sources` in the cabal file, run:

    $ ack -a -f --cc src/cbits | grep -v test | grep -v "\.h\$" | grep "\.cc\$" | sort

