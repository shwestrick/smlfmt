signature S = SEQUENCE
val x = 5
signature X = Y

signature Foo = sig end
signature Bar = Foo where type 'a t = 'a list -> 'a -> 'a


(** This is valid in the grammar but is rejected by SML/NJ, because SML/NJ
  * incorrectly assumes the "and" begins "and type ...".
  * MLton accepts it though!
  *)
signature X = sig end where type t = int
and Y = sig end


signature FooBar = Bar
  where type 'a Baz.Bingo.yeah = 'a list -> 'a
    and type 'a z = 'a t
  where type ('a, 'b) u = ('a -> 'b -> string) -> string list
and Baz = sig end where type string = String.string
