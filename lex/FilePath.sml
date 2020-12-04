structure FilePath:
sig
  type filepath
  type t = filepath

  exception InvalidPathString of string

  val fromUnixPath: string -> filepath
  val toUnixPath: filepath -> string
  val toHostPath: filepath -> string

  val isAbsolute: filepath -> bool
end =
struct

  (** Path fields in reverse order *)
  type filepath = string list
  type t = filepath

  exception InvalidPathString of string

  fun fromUnixPath s =
    if String.size s = 0 then
      raise InvalidPathString s
    else
      List.rev (String.fields (fn c => c = #"/") s)

  fun toUnixPath s =
    String.concatWith "/" (List.rev s)

  (** The first field of an (absolute) Unix path is always the empty string.
    * So look at the last, because we represent path fields in reverse order.
    *)
  fun isAbsolute path =
    List.last path = ""

  fun toHostPath path =
    OS.Path.fromUnixPath (toUnixPath path)
end
