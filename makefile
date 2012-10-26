GITVERSION="40768657bc8ec3ded60712eeeab7c25b1b07deca"
GITURL=https://code.google.com/p/leveldb/
OUT=$(shell pwd)/src/cbits/leveldb

Q?=@

update:
	$(Q)echo -n 'Downloading LevelDB... '
	$(Q)git clone $(GITURL) .leveldb-src
	$(Q)cd .leveldb-src && git checkout $(GITVERSION)
	$(Q)echo OK

	$(Q)echo -n 'Copying source code into $(OUT)...'
	$(Q)rm -rf $(OUT)
	$(Q)mkdir $(OUT)
	$(Q)cp -r .leveldb-src/{db,include,port,table,util} $(OUT)
	$(Q)rm -rf .leveldb-src
	$(Q)echo OK

# Give us the list of updated .c files:
	$(Q)echo The following is the list of all .c files copied. Be sure to update whistlepig.cabal:
	-$(Q)ack -a -f --cc src/cbits | grep -v test | grep -v "\.h\$" | grep "\.cc\$" | sort
	$(Q)echo Done!

clean:
	rm -rf *~ dist*
distclean: clean
	rm -rf $(OUT)
	mkdir $(OUT)
