ack -a -f --ignore-dir=src/Database --ignore-dir=src/cbits/leveldb/include --cpp src | grep -v \.h$ | grep -v test | grep -v _test\.c\*$ | grep -v android | grep \.cc$
