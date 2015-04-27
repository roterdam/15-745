all: l5c

l5c:
	runhaskell Setup.hs configure --user
	runhaskell Setup.hs build
	rm -rf bin/
	cp -r shellbins/ bin/
	chmod +x bin/*

prof:
	runhaskell Setup.hs configure --user --enable-executable-profiling
	runhaskell Setup.hs build --ghc-options="-O2 -prof"
	rm -rf bin/
	cp -r shellbins/ bin/
	chmod +x bin/*

clean:
	rm -rf bin/
	rm -rf dist/
