
val x = 5;

fun foo (x, y) = "hello"
  | foo _ = "goodbye";

type int = string

datatype X = XX of int t
withtype 'a t = 'a list

exception foobar of string

local
  datatype Ya = Boi
in
val x: Ya = Boi
end

open X Y Foo.Z

abstype
  'a foo = Foo of 'a | Bar of 'a * 'a
with
  fun foo x = Foo x
  fun bar x = Bar (x,x)
end
