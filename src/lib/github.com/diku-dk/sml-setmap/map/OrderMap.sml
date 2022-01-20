(* Finite maps using balanced AVL trees *)

functor OrderMap (Order : sig type t val compare: t*t->order end)
        : MONO_MAP =
        OrderMapImpl(Order)
