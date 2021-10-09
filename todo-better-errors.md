# Error messages that could be better

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
