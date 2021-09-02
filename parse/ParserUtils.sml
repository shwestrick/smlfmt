structure ParserUtils:
sig
  exception Error of LineError.t
  val error: {what: string, pos: Source.t, explain: string option} -> 'a

  val errorIfInfixNotOpped: InfixDict.t -> Token.t option -> Token.t -> unit
end =
struct

  exception Error of LineError.t

  fun error {what, pos, explain} =
    raise Error
      { header = "PARSE ERROR"
      , pos = pos
      , what = what
      , explain = explain
      }

  fun errorIfInfixNotOpped infdict opp vid =
    if InfixDict.contains infdict vid andalso not (Option.isSome opp) then
      error
        { pos = Token.getSource vid
        , what = "Infix identifier not prefaced by 'op'"
        , explain = NONE
        }
    else
      ()

end
