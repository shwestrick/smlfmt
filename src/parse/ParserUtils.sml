(** Copyright (c) 2020-2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure ParserUtils:
sig
  val error: {what: string, pos: Source.t, explain: string option} -> 'a

  val errorIfInfixNotOpped: InfixDict.t -> Token.t option -> Token.t -> unit

  val nyi: Token.t Seq.t -> string -> int -> 'a
end =
struct

  fun error {what, pos, explain} =
    raise Error.Error (Error.LineError
      { header = "PARSE ERROR"
      , pos = pos
      , what = what
      , explain = explain
      })

  fun errorIfInfixNotOpped infdict opp vid =
    if InfixDict.isInfix infdict vid andalso not (Option.isSome opp) then
      error
        { pos = Token.getSource vid
        , what = "Infix identifier not prefaced by 'op'"
        , explain = NONE
        }
    else
      ()


  fun nyi toks fname i =
    if i >= Seq.length toks then
      raise Error.Error (Error.LineError
        { header = "ERROR: NOT YET IMPLEMENTED"
        , pos = Token.getSource (Seq.nth toks (Seq.length toks - 1))
        , what = "Unexpected EOF after token."
        , explain = SOME ("(TODO: see parser " ^ fname ^ ")")
        })
    else if i >= 0 then
      raise Error.Error (Error.LineError
        { header = "ERROR: NOT YET IMPLEMENTED"
        , pos = Token.getSource (Seq.nth toks i)
        , what = "Unexpected token."
        , explain = SOME ("(TODO: see parser " ^ fname ^ ")")
        })
    else
      raise Fail ("Bug in parser " ^ fname ^ ": position out of bounds??")


end
