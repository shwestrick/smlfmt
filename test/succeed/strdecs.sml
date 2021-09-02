structure X = Y
structure Z = X.Y.Foo
and Bar = X.Y
and Baz = Z

structure X: Foo = Y

structure Y:
sig
  type t
  type u
  datatype 'a x = Foo | Bar
  val x: t
  val y: u -> t
end
where type t = int
  and type u = unit
= FooBar

and Z :> YO = Ya
