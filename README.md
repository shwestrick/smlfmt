# parse-sml

A custom lexer/parser for Standard ML with the goal of providing
better error messages. Supports standard SML source files
(`.sml`, `.sig`, `.fun`, etc.) as well as
[MLBasis](http://mlton.org/MLBasis) compilation files (`.mlb`) using
[MLton](https://github.com/MLton/mlton) conventions,
including [MLBasis path maps](http://mlton.org/MLBasisPathMap).

## Examples

```
$ ./main test/fail/illegal-begin-fn.sml
-- PARSE ERROR --------------------------------------------

Unexpected beginning of anonymous function.

test/fail/illegal-begin-fn.sml
  |
2 | val id' = id o fn x => x
  |                ^^

Try using parentheses: (fn ... => ...)
```

```
$ ./main test/fail/bad-let-in-end-3.sml
-- PARSE ERROR --------------------------------------------

Unexpected token.

test/fail/bad-let-in-end-3.sml
  |
4 |     val y = let in (1, 2) end end
  |                               ^^^

Expected to see 'in' or another declaration.

The error occurred inside of this 'let':

test/fail/bad-let-in-end-3.sml
  |
2 |   let
  |   ^^^
```

```
$ ./main test/fail/bad-real.sml
-- SYNTAX ERROR -------------------------------------------

Invalid real constant.

test/fail/bad-real.sml
  |
1 | val x = 1.E1
  |         ^^

After the dot, there needs to be at least one decimal digit.
```

## Build and run

You need [`mlton`](http://mlton.org/) installed.

Do `make` and then pass either a `.sml` file or a `.mlb` file, for example:
```bash
$ make
$ ./main src/lex/Token.sml
$ ./main test/succeed/full-sml-basis-library.mlb
```

### Command-line options

`-mlb-path-var '<key> <value>'` for handling path variables, similar to
[MLton's path maps](http://mlton.org/MLBasisPathMap).
