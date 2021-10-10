structure StringDoc =
  PrettySimpleDoc(struct
    open String
    type t = string
    fun fromString x = x
  end)
