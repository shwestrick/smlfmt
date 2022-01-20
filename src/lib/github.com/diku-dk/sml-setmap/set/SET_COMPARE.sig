(** Base signature for polymorphic sets.

The SET_COMPARE signature is a generic interface to polymorphic sets
and is the basis for SET_EQ and SET_ORDER.
*)

signature SET_COMPARE = sig
  type 'a t
  type cmp

  val empty      : unit -> 'a t
  val singleton  : 'a -> 'a t
  val size       : 'a t -> int
  val isEmpty    : 'a t -> bool
  val member     : ('a * 'a -> cmp) -> 'a * 'a t -> bool
  val eq         : ('a * 'a -> cmp) -> 'a t * 'a t -> bool
  val list       : 'a t -> 'a list
  val fromList   : ('a * 'a -> cmp) -> 'a list -> 'a t
  val insert     : ('a * 'a -> cmp) -> 'a * 'a t -> 'a t
  val remove     : ('a * 'a -> cmp) -> 'a * 'a t -> 'a t
  val difference : ('a * 'a -> cmp) -> 'a t * 'a t -> 'a t
  val intersect  : ('a * 'a -> cmp) -> 'a t * 'a t -> 'a t
  val union      : ('a * 'a -> cmp) -> 'a t * 'a t -> 'a t
  val partition  : ('a -> bool) -> 'a t -> 'a t * 'a t
  val fold       : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val app        : ('a -> unit) -> 'a t -> unit
end

(**

[empty ()] returns the empty set of elements.

[singleton e] returns the singleton set containing the element e.

[size s] returns the cardinality of the set s.

[isEmpty s] returns true if the set s is empty. Returns false
otherwise.

[member cmp (e,s)] returns true if the element e appears in s
according to cmp. Returns false otherwise.

[eq cmp (a,b)] returns true if the two sets a and b contains the same
set of elements, according to cmp. Returns false otherwise.

[list s] returns the elements of s as a list.

[fromList cmp l] returns the elements in l as a set according to the
relation cmp.

[insert cmp (e,s)] returns the set s with the element e added
according to the relation cmp.

[remove cmp (e,s)] returns the set s with the element e removed
according to the relation cmp.

[difference cmp (a,b)] returns the set a with the elements of b
removed according to the relation cmp.

[intersect cmp (a,b)] returns the set of elements that appear in both
a and b according to the relation cmp.

[union cmp (a,b)] returns the set containing the elements from a and
b according to the relation cmp.

[partition f a] returns a pair of sets (b,c) where b contains elements
for which f is true and c contains elements for which f is false.

[fold f e s] folds over the elements of the set s using the function f
and e as the initial accumulated value.

[app f s] applies the function f to each element in s.

*)
