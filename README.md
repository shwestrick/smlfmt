# parse-sml

A custom lexer/parser/formatter for Standard ML which also offers
better error messages.

Supports SML source files
(`.sml`, `.sig`, `.fun`, etc.) as well as
[MLBasis](http://mlton.org/MLBasis) compilation files (`.mlb`) using
[MLton](https://github.com/MLton/mlton) conventions,
including [MLBasis path maps](http://mlton.org/MLBasisPathMap).

## Examples: Error Messages

![Example 1](examples/ex1-small.png)

![Example 2](examples/ex2-small.png)

![Example 3](examples/ex3-small.png)

## Examples: Code Formatting

The formatter (which is usable, but is still undergoing aesthetic
improvements) generally handles comments in a reasonable way.

Input:
```sml
fun fib n = (* there are
             * two base cases, but these
             * can be compressed into one *) if n < 2 then n
  else fib (n-1)
  + fib (n-2)
```

Output:
```sml
fun fib n =
      (* there are
       * two base cases, but these
       * can be compressed into one *)
      if n < 2 then
        n
      else
        fib (n - 1) + fib (n - 2)
```

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

To see formatted output on a `.sml` (or `.sig`, `.fun`, etc.) file, you can
pass `--pretty`. (Note: the formatter is usable but still is undergoing
aesthetic improvements.)

### Command-line options

`-mlb-path-var '<key> <value>'` for handling path variables, similar to
[MLton's path maps](http://mlton.org/MLBasisPathMap).

`--no-skip-basis` to also parse standard basis files.

`--pretty` to also see autoformatted output (only for SML, not MLB)

`-ribbon-frac <real>` controls pretty-printing. The
ribbon-frac (between 0 and 1) controls how dense each line is, excluding
indentation. Low ribbon-fracs will have very little non-whitespace content
on each line, whereas high ribbon-fracs will attempt to fill the line as
much as possible. Default value = 0.5.

`-max-width <int>` controls pretty-printing. This is the desired maximum number
of columns in each line. Default value = 80.
