# parse-sml (WIP)

A custom lexer/parser for Standard ML with the goal of providing
better error messages. Perhaps it could also eventually provide SML tooling
such as linting and autoformatting. It would be great also to have a
comprehensive language server for SML.

## Current Status

The lexer is done, and now we're working on the parser. See the TODO list below.

For files that pass through lexing successfully, you will see a bit of syntax
highlighting in the output. Files that are parsed successfully will be then
also be pretty-printed.

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

To see syntax highlighting, you'll need a terminal that supports colors via
ANSI escapes, e.g. iTerm2 on macOS.

## Core Parsing TODO

Patterns:
- [x] wildcard (`_`)
- [x] constants (unit, strings, chars, integers, etc.)
- [x] tuples
- [x] identifiers
- [x] parentheses
- [x] `op` identifiers (e.g. `op+`)
- [x] constructed values (e.g. `Node (a, b)`)
- [x] infixed constructed values (e.g. `x :: xs`)
- [x] list sugar (`[a, b, c]`)
- [x] records
- [x] type annotations (e.g. `x: ty`)
- [x] layered patterns (e.g. `x as (a, b)`)

Types:
- [x] type variables (e.g. `'a`)
- [x] type constructors (e.g. `int list`)
- [x] products
- [x] arrows
- [x] parentheses
- [x] records

Expressions
- [x] constants (unit, strings, chars, integers, etc.)
- [x] tuples
- [x] parentheses
- [x] sequences (i.e. `(a; b; c)`)
- [x] list sugar
- [x] records
- [x] record selectors
- [x] identifiers
- [x] `op` identifiers
- [x] let-in-end
- [x] infix with precedences
- [x] `andalso`, `orelse`
- [x] function application
- [x] function constants (`fn ... => ...`)
- [x] type annotations (e.g. `5: int`)
- [x] cases
- [x] handle exception
- [x] raise exception
- [x] if-then-else
- [x] while-do

Declarations:
- [x] value bindings (not mutually-recursive)
- [x] mutually-recursive values
- [x] type bindings (not mutually-recursive)
- [x] mutually-recursive types
- [x] function value bindings (not mutually-recursive, single pattern set)
- [x] multiple pattern-sets for `fun`
- [x] mutually-recursive function value bindings
- [x] local infix directives (`infix`, `infixr`, `nonfix`)
- [x] multiple declarations in sequence
- [x] datatypes
- [x] datatype replications (e.g. `datatype x = datatype y`)
- [x] exceptions
- [x] local-in-end
- [ ] **open structure**
- [ ] **abstype**

## Module Parsing TODO

Signature Expressions:
- [x] identifiers (`signature X = Y`)
- [ ] sig-end (`sig ... end`)
- [ ] where-type realizations (e.g. `S where type a = b`)

Signature Specifications:
- [ ] value
- [ ] type
- [ ] eqtype
- [ ] datatypes
- [ ] datatype replications
- [ ] exceptions
- [ ] structures (`structure X: S`)
- [ ] include
- [ ] multiple specs in sequence
- [ ] sharing-type (e.g. `spec sharing type a = b = c`)

Structure Expressions
- [ ] identifiers
- [ ] struct-end
- [ ] transparent constraint
- [ ] opaque constraint
- [ ] functor application
- [ ] let-in-end

Top-level "strdecs":
- [x] core language declarations
- [ ] structures (`structure X = ...`)
- [ ] local-in-end
- [ ] multiple strdecs in sequence

Other Top-level Declarations:
- [x] signatures (`signature X = ...`)
- [ ] functors (`functor F(...) = ...`)
- [x] multiple in sequence

Derived Forms
- TODO...
