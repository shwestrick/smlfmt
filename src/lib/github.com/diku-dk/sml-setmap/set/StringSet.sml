
structure StringSet : MONO_SET =
OrderSet(type t = string
         val compare = String.compare)
