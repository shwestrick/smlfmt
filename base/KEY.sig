(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

signature KEY =
sig
  type t
  val equal: t * t -> bool
end
