# parse-sml

A custom lexer/parser for Standard ML with the goal of providing
better error messages. Perhaps it could also eventually provide SML tooling
such as linting, autoformatting or even a language server.

## Current Status

Lexer and parser are done! (There may be some bugs to iron out, though.)

For files that pass through lexing successfully, you will see a bit of syntax
highlighting in the output. Files that are parsed successfully are
also pretty-printed.

Next, we are working on good error reporting...

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

To see syntax highlighting, you'll need a terminal that supports colors via
ANSI escapes, e.g. iTerm2 on macOS.
