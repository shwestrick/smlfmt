(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

signature LEX =
sig
  exception Error of LineError.t

  (** Get the next token in the given source. If there isn't one, returns NONE.
    * raises Error if there's a problem.
    *)
  val next: Source.t -> Token.t option

  (** Get all the tokens in the given source.
    * raises Error if there's a problem.
    *)
  val tokens: Source.t -> Token.t Seq.t
end
