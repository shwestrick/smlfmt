(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure LexResult =
struct

  datatype error =
    Other of string
  | Error of {pos: Source.t, what: string, explain: string option}

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

    | Error {pos, what, explain} =>
        let
          val lineErr = ShowLineError.show pos what
        in
          case explain of
            NONE => lineErr
          | SOME msg => lineErr ^ msg
        end

  datatype result =
    Success of Token.t Seq.t
  | Failure of {partial: Token.t Seq.t, error: error}

  type t = result

end
