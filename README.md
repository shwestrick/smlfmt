# parse-sml

A custom lexer/parser for Standard ML with the goal of providing
better error messages. Supports standard SML source files
(`.sml`, `.sig`, `.fun`, etc.) as well as
[MLBasis](http://mlton.org/MLBasis) compilation files (`.mlb`) using
[MLton](https://github.com/MLton/mlton) conventions,
including [MLBasis path maps](http://mlton.org/MLBasisPathMap).

## Examples

![Example 1](examples/ex1-small.png)

![Example 2](examples/ex2-small.png)

![Example 3](examples/ex3-small.png)

## Build and run

You need [`mlton`](http://mlton.org/) installed.

Do `make` and then pass either a `.sml` file or a `.mlb` file, for example:
```bash
$ make
$ ./main src/main.sml
$ ./main test/succeed/full-sml-basis-library.mlb --no-skip-basis
```

When given a `.mlb`, the default behavior is to ignore any file in the
installed standard basis library. Passing `--no-skip-basis` overrides this.

### Command-line options

`-mlb-path-var '<key> <value>'` for handling path variables, similar to
[MLton's path maps](http://mlton.org/MLBasisPathMap).

`--no-skip-basis` to also parse standard basis files.
