structure StringMap : MONO_MAP =
  OrderMap(struct type t = string
                  val compare = String.compare
           end)
