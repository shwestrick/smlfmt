# parse-sml (WIP)

I'm building a custom lexer/parser for Standard ML with the goal of providing
better error messages. Perhaps it could also eventually provide SML tooling.
(I would love to have a comprehensive language server for SML...)

## Current Status

The lexer is done, and now I'm working on the parser. See the TODO list below.

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

## Core Parsing TODO

Progress on the core language (no modules yet):

Patterns:
- [x] wildcard (`_`)
- [x] constants (unit, strings, chars, integers, etc.)
- [x] tuples
- [x] identifiers
- [x] parentheses
- [ ] `op` identifiers (e.g. `op+`)
- [ ] constructed values (e.g. `Node (a, b)`)
- [ ] infixed constructed values (e.g. `x :: xs`)
- [ ] list sugar (`[a, b, c]`)
- [ ] records
- [ ] type annotations (e.g. `x: ty`)
- [ ] layered patterns (e.g. `x as (a, b)`)

Types:
- [x] type variables (e.g. `'a`)
- [x] type constructors (e.g. `int list`)
- [x] products
- [x] arrows
- [x] parentheses
- [ ] records

Expressions
- [x] constants (unit, strings, chars, integers, etc.)
- [x] tuples
- [x] parentheses
- [x] sequences (i.e. `(a; b; c)`)
- [ ] list sugar
- [ ] records
- [ ] record selectors
- [x] identifiers
- [x] `op` identifiers
- [x] let-in-end
- [x] infix with precedences
- [x] `andalso`, `orelse`
- [x] function application
- [x] function constants (`fn ... => ...`)
- [x] type annotations (e.g. `5: int`)
- [ ] cases
- [ ] handle exception
- [x] raise exception
- [ ] if-then-else
- [ ] while-do

Declarations:
- [x] value bindings (not mutually-recursive)
- [ ] mutually-recursive values
- [x] type bindings (not mutually-recursive)
- [ ] mutually-recursive types
- [x] function value bindings (not mutually-recursive, single pattern set)
- [ ] multiple pattern-sets for `fun`
- [ ] mutually-recursive function value bindings
- [x] local infix directives (`infix`, `infixr`, `nonfix`)
- [x] multiple declarations in sequence
- [ ] datatypes
- [ ] datatype replications (e.g. `datatype x = datatype y`)
- [ ] exceptions
- [ ] open structure
- [ ] local-in-end
- [ ] abstype
