# parse-sml

A custom lexer/parser for Standard ML with the goal of providing
better error messages. Perhaps it could also eventually provide SML tooling
such as linting, autoformatting or even a language server.

## Examples

```
$ ./main test/fail/reserved-in-long-id.sml
-- SYNTAX ERROR --------- test/fail/reserved-in-long-id.sml

Unexpected reserved keyword.

5| val z = X.and.b
             ^^^
Reserved keywords cannot be used as identifiers.
```

```
$ ./main test/fail/bad-real.sml
-- SYNTAX ERROR -------------------- test/fail/bad-real.sml

Invalid real constant.

1| val x = 1.E1
           ^^
After the dot, there needs to be at least one decimal digit.
```

```
$ ./main test/fail/dots.sml
-- SYNTAX ERROR ------------------------ test/fail/dots.sml

Unexpected character.

2| val {z, ..} = yo
             ^
Perhaps you meant: ...
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
also pretty-printed.

To see syntax highlighting, you need a terminal that supports colors via
ANSI escapes, e.g. iTerm2 on macOS.

### Command-line options

Passing `--errors-only` will prevent any output except for a lex/parse error,
if there is one. Files that successfully parse will have no output.
