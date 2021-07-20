signature S = SEQUENCE
val x = 5
signature X = Y

signature Foo = sig end
signature Bar = Foo where type 'a t = 'a list -> 'a -> 'a

signature FooBar = Bar
  where type 'a Baz.Bingo.yeah = 'a list -> 'a
    and type 'a z = 'a t
  where type ('a, 'b) u = ('a -> 'b -> string) -> string list
