# cplot

Live 2D function plotting tool.

## Installation

### Haskell Ecosystem

Standard `cabal` and `stack` mechanisms are supported.

### Debian and Debian Derivatives

Not currently supported until/unless packages for `megaparsec` v6 and `frpnow`
are made available. As a result, the Makefile won't work.

<!-- The following packages need to be installed:

* `ghc`
* `libghc-gloss-dev`
* `libghc-attoparsec-dev`
* `libghc-pipes-bytestring-dev`

At this point, `make` should generate a bloated executable `cplot` which can be
installed in `/usr/local/bin/`. Intermediate `.hi` and `.o` files are sent to
`/tmp/`. -->

## Usage

Try

````shell
$ yes | gawk '@load "time"; {sleep(0.2); print NR, NR*sqrt(NR)}' | ./cplot
````

If this produces no output, you may need to flush stdout after each print.

```shell
$ yes | gawk '@load "time"; {sleep(0.2); print NR, NR*sqrt(NR); fflush()}' | ./cplot
```

## License

See the [LICENSE](https://github.com/SilverSylvester/cplot/blob/master/LICENSE).
