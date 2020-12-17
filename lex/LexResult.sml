(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure LexResult =
struct

  structure Error =
  struct
    datatype error =
      Other of string
    | UnexpectedDot of {pos: Source.t}
    | UnexpectedCloseComment of {pos: Source.t}
    | ReservedAsIdentifier of {pos: Source.t}
    | QualifierStartsPrime of {pos: Source.t}
    | QualifiedIdentifierStartsPrime of {pos: Source.t}
    | UnexpectedEndOfLongIdentifier of {pos: Source.t}
    | InvalidRealConstant of {pos: Source.t, reason: string}
    | UnclosedString of {pos: Source.t}

    type t = error

    fun lcToStr {line, col} =
      "(line "
      ^ Int.toString line
      ^ ", col "
      ^ Int.toString col
      ^ ")"

    fun report err =
      case err of
        Other msg =>
          "Uncategorized error: " ^ msg

      | UnexpectedCloseComment {pos} =>
          ShowLineError.show pos "unexpected end-of-comment symbol"

      | UnexpectedDot {pos} =>
          ShowLineError.show pos "unexpected dot"

      | ReservedAsIdentifier {pos} =>
          ShowLineError.show pos "reserved word cannot be used as identifier"

      | QualifierStartsPrime {pos} =>
          ShowLineError.show pos "structure identifiers cannot start with prime"

      | QualifiedIdentifierStartsPrime {pos} =>
          ShowLineError.show pos "unexpected prime after a qualifier."

      | UnexpectedEndOfLongIdentifier {pos} =>
          ShowLineError.show pos "unexpected character"
          ^ "In a long identifier, after a dot, there needs to be either "
          ^ "a letter or symbol."

      | InvalidRealConstant {pos, reason} =>
          ShowLineError.show pos "invalid real constant"
          ^ reason

      | UnclosedString {pos} =>
          ShowLineError.show pos "unclosed string"
  end

  datatype result =
    Success of Token.t Seq.t
  | Failure of {partial: Token.t Seq.t, error: Error.t}

  type t = result

end
