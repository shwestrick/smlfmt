(** Monomorphic finite maps.

The MONO_MAP signature is a generic interface to monomorphic finite
maps.
*)

signature MONO_MAP = sig
  type dom
  type 'b map

  val empty      : 'a map
  val singleton  : dom * 'a -> 'a map
  val isEmpty    : 'a map -> bool
  val lookup     : 'a map -> dom -> 'a option
  val add        : dom * 'a * 'a map -> 'a map
  val plus       : 'a map * 'a map -> 'a map
  val remove     : dom * 'a map -> 'a map option
  val dom        : 'a map -> dom list
  val range      : 'a map -> 'a list
  val list       : 'a map -> (dom * 'a) list
  val fromList   : (dom * 'a) list -> 'a map
  val map        : ('a -> 'b) -> 'a map -> 'b map
  val Map        : (dom * 'a -> 'b) -> 'a map -> 'b map
  val fold       : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
  val Fold       : ((dom * 'a) * 'b -> 'b) -> 'b -> 'a map -> 'b
  val filter     : (dom * 'b -> bool) -> 'b map -> 'b map
  val addList    : (dom * 'b) list -> 'b map -> 'b map
  val merge      : ('a * 'a -> 'a) -> 'a map -> 'a map -> 'a map

  exception Restrict of dom
  val restrict   : 'b map * dom list -> 'b map
  val enrich     : ('b * 'b -> bool) -> 'b map * 'b map -> bool
end

(**
[addList l m] adds a list of associations to a map.

[merge f m1 m2] merges two finite maps, with a composition function
to apply to the codomains of domains which clash.

[restrict (m,d)] returns a map with domain d and values as in m.
Raises exception Restrict if an element of the list is not in the
domain of the map.

[enrich en (A, B)] returns true if for all a and b such that b \in B
and a \in (A \restrict dom(B)) we have en(a,b).

*)
