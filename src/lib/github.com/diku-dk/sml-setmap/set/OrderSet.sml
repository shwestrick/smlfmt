functor OrderSet(Order: sig type t val compare: t*t->order end) : MONO_SET =
    OrderSetImpl(Order)
