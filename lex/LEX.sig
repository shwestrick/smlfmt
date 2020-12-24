(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

signature LEX =
sig
  val next: Source.t -> (Token.t, LineError.t) MaybeError.t option
  val tokens: Source.t -> (Token.t Seq.t, LineError.t) MaybeError.t
end
