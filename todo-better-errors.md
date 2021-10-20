# Error messages that could be better

## Equal-sign looks like identifier

**Problem**: The parser tries to use '=' as the name of a variable, but gets
mad because '=' is infixed but does not appear in infix position. Perhaps
another reasonable error would be to say that '=' cannot be rebound.

**Ideal Solution**: Say something like "missing variable or pattern binder" and
point to the whitespace, if possible.

```
Infix identifier not prefaced by 'op'

input.sml
  |
1 | val = 5
  |     ^
```

## Double comma/semicolon

**Problem**: A common mistake when writing lists or tuples or sequences of
expressions is having two commas/semicolons in a row. The parser currently
doesn't do anything to detect this.

**Ideal Solution**: Explicitly say "two X in a row" and point to both.

```
Unexpected token.

test.sml
  |
3 |   , foo
  |   ^

Expected beginning of expression.
```

## Extra comma/semicolon at end

**Problem**: Similar to the double comma/semicolon problem, it's common to
accidentally put a superfluous comma or semicolon at the end of a
tuple/list/sequence.

**Ideal Solution**: Explicitly point to the extra comma/semicolon.

```
Unexpected token.

test.sml
  |
1 | val x = (a, b, c, )
  |                   ^

Expected beginning of expression.
```
