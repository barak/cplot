all: cplot _diary/diary.pdf

GHC=ghc
GHCFLAGS := $(shell egrep ghc-options cplot.cabal | head -1 | sed 's/ghc-options://')

cplot: app/Main.hs
	$(GHC) $(GHCFLAGS) $^ -o $@

_diary/diary.pdf: _diary/diary.tex
	cd _diary && latexmk -shell-escape --pdf diary.tex
