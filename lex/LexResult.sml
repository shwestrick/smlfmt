(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure LexResult =
struct

  structure Error =
  struct
    datatype error =
      (*UnexpectedCloseComment of {pos: Source.t}
    | EndOfFileUnclosedComment of {start: Source.t}
    | EndOfFileUnclosedString of {start: Source.t}
    |*) Other of string

    type t = error
  end

  datatype result =
    Success of Token.t Seq.t
  | Failure of {partial: Token.t Seq.t, error: Error.t}

  type t = result

end
