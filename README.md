# parse-sml

A custom lexer/parser for Standard ML with the goal of providing
better error messages.

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
$ ./main test/fail/reserved-in-long-id.sml
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

Do `make` and then pass a `.sml` file, for example:
```
$ make
$ ./main lex/Token.sml
```

For files that pass through lexing successfully, you will see a bit of syntax
highlighting in the output. Files that are parsed successfully are
also pretty-printed. To see syntax highlighting, you need a terminal that
supports colors via ANSI escapes, e.g. iTerm2 on macOS.

You can also pass a `.mlb` file:
```
$ ./main test/succeed/multiple-tests.mlb
```
This will find all SML source files specified by the MLB and parse them.

### Command-line options

Passing `--errors-only` will prevent any output except for a lex/parse error,
if there is one. Files that successfully parse will have no output.

`-mlb-path-var '<key> <value>'` for handling path variables, similar to
[MLton's path maps](http://mlton.org/MLBasisPathMap).
