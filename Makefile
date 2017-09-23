GHC=ghc
GHCFLAGS := $(shell egrep ghc-options cplot.cabal | head -1 | sed 's/ghc-options://')

cplot: app/Main.hs
	$(GHC) $(GHCFLAGS) $^ -o $@
