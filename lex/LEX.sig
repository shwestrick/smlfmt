(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

signature LEX =
sig
  val tokens: Source.t -> LexResult.t
end
