(** Monomorphic sets.

The MONO_SET signature is a generic interface to monomorphic sets.
*)

signature MONO_SET = sig
  type set
  type elem

  val empty      : set
  val singleton  : elem -> set
  val size       : set -> int
  val isEmpty    : set -> bool
  val member     : elem * set -> bool
  val eq         : set * set -> bool
  val list       : set -> elem list
  val fromList   : elem list -> set
  val insert     : elem * set -> set
  val remove     : elem * set -> set
  val difference : set * set -> set
  val intersect  : set * set -> set
  val union      : set * set -> set
  val partition  : (elem -> bool) -> set -> set * set
  val fold       : (elem * 'b -> 'b) -> 'b -> set -> 'b
  val app        : (elem -> unit) -> set -> unit
end

(**

[addList (s,l)] adds elements in list l to s.

[fold f base s] folds using f over the base element.

[app f s] applies f to each element of s (in order).

*)
