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


structure Hello :>
sig
  val hello: string -> unit
end =
struct
  fun hello x = print (x ^ "\n")
end

signature X = sig type t end
signature Y = sig end

structure S :> Y =
  struct type t = int end : sig type t end : sig end
structure S =
  struct type t = int end : X : Y

structure S = F(X)
structure T :> Y = Fun (struct type t = int val x: int = 5 end) : X

structure X =
  F (type t = int
     val x: t = 5
     datatype P = L | R)

(** uncomment this as soon as sequenced multiple strdecs are added to
  * to parser.
  *)
(* structure S = F (structure X:Foo = X val hello: string = "hello") *)
