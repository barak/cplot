# cplot [WIP]

Live 2D plotting tool.

This project is a work in progress; as such, it's not at all guaranteed at any
given moment that this will even *build*, much less do what you want.

## Installation

### Haskell Ecosystem

Standard `cabal` and `stack` mechanisms are supported.

### Debian and Debian Derivatives

Not currently supported until/unless up-to-date packages are made available for
all libs used in this project. As a result, the Makefile won't work.

## Usage

Try

````shell
$ yes | gawk '@load "time"; {sleep(0.2); print NR, NR*sqrt(NR)}' | ./cplot -c line
````

If this produces no output, you may need to flush stdout after each print.

```shell
$ yes | gawk '@load "time"; {sleep(0.2); print NR, NR*sqrt(NR); fflush()}' | ./cplot -c line
```

## License

See the [LICENSE](https://github.com/SilverSylvester/cplot/blob/master/LICENSE).
