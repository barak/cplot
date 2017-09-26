GHC=ghc
GHCFLAGS := $(shell egrep ghc-options cplot.cabal | head -1 | sed 's/ghc-options://')
SRCFILES := $(shell find src/ -type f -name '*.hs')

cplot: app/Main.hs $(SRCFILES)
	$(GHC) $(GHCFLAGS) -outputdir /tmp/ $^ -o $@
