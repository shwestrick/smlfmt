structure LexResult =
struct

  datatype error =
    (*UnexpectedCloseComment of {pos: Source.t}
  | EndOfFileUnclosedComment of {start: Source.t}
  | EndOfFileUnclosedString of {start: Source.t}
  |*) OtherError of string

  datatype result =
    Success of Token.t Seq.t
  | Failure of {partial: Token.t Seq.t , error: error}

  type t = result

end
