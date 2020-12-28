# parse-sml (WIP)

I'm building a custom lexer/parser for Standard ML with the goal of providing
better error messages. Perhaps it could also eventually provide SML tooling.
(I would love to have a comprehensive language server for SML...)

## Current Status

The lexer is done, and now I'm working on the parser.

For files that pass through lexing successfully, you will see a bit of syntax
highlighting in the output.

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

You'll need [`mlton`](http://mlton.org/) installed.

Do `make` and then pass a `.sml` file, for example:
```
$ make
$ ./main lex/Lexer.sml
```

This lexes the file and outputs it with highlighting to indicate token
classes. You'll need a terminal that supports colors via ANSI escapes, e.g.
iTerm2 on macOS.

## Design rationale

I decided not to use mllex/mlyacc for a number of reasons.
  * Using mllex/mlyacc would overly restrict the behavior of lexing and parsing.
  * The SML grammar is set in stone! This project doesn't need to specify or
  clarify the grammar itself---there are plenty of existing resources available
  for that.
  * I've always wanted to write a lexer and parser from scratch. Implementing
  finite state-machines (even large ones!) with a bunch of mutually recursive
  functions is fairly straightforward and pretty fun. The invariants of each
  state are easy to write down, and the code is somewhat self-documenting.
