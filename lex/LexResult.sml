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
          lcToStr (Source.absoluteStart pos)
          ^ " Unexpected end-of-comment symbol."

      | UnexpectedDot {pos} =>
          lcToStr (Source.absoluteStart pos)
          ^ " Unexpected dot."

      | ReservedAsIdentifier {pos} =>
          lcToStr (Source.absoluteStart pos)
          ^ " Reserved word: "
          ^ Source.toString pos
          ^ "\nReserved words cannot be used as identifiers."

      | QualifierStartsPrime {pos} =>
          lcToStr (Source.absoluteStart pos)
          ^ " Unexpected prime: "
          ^ Source.toString pos
          ^ "\nStructure identifiers cannot start with a prime."

      | QualifiedIdentifierStartsPrime {pos} =>
          lcToStr (Source.absoluteStart pos)
          ^ " Unexpected prime after a qualifier."

      | UnexpectedEndOfLongIdentifier {pos} =>
          lcToStr (Source.absoluteStart pos)
          ^ " Long identifier ends unexpectedly."
          ^ "\nAfter the dot, there needs to be either a letter or a symbol."

      | InvalidRealConstant {pos, reason} =>
          lcToStr (Source.absoluteStart pos)
          ^ " Invalid real constant: "
          ^ Source.toString pos
          ^ "\n" ^ reason

      | UnclosedString {pos} =>
          lcToStr (Source.absoluteStart pos)
          ^ " Unclosed string at end of line."
  end

  datatype result =
    Success of Token.t Seq.t
  | Failure of {partial: Token.t Seq.t, error: Error.t}

  type t = result

end
