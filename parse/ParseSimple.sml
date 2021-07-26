(** Copyright (c) 2020 Sam Westrick
  * See the file LICENSE for details.
  *
  * This structure implements a lot of the "simple" parser functions, to
  * avoid cluttering the main Parser implementation.
  *)
structure ParseSimple:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type 'a peeker = 'a ParserCombinators.peeker

  exception Error of LineError.t

  val error: {what: string, pos: Source.t, explain: string option} -> 'a

  type tokens = Token.t Seq.t

  val reserved: tokens -> Token.reserved -> (int, Token.t) parser
  val maybeReserved: tokens -> Token.reserved -> (int, Token.t option) parser
  val tyvar: tokens -> (int, Token.t) parser
  val sigid: tokens -> (int, Token.t) parser
  val vid: tokens -> (int, Token.t) parser
  val longvid: tokens -> (int, Ast.MaybeLong.t) parser
  val recordLabel: tokens -> (int, Token.t) parser
end =
struct

  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type 'a peeker = 'a ParserCombinators.peeker
  type tokens = Token.t Seq.t

  exception Error of LineError.t


  fun error {what, pos, explain} =
    raise Error
      { header = "PARSE ERROR"
      , pos = pos
      , what = what
      , explain = explain
      }


  fun check toks f i =
    let
      val numToks = Seq.length toks
      fun tok i = Seq.nth toks i
    in
      i < numToks andalso f (tok i)
    end


  fun isReserved toks rc i =
    check toks (fn t => Token.Reserved rc = Token.getClass t) i


  fun reserved toks rc i =
    if isReserved toks rc i then
      (i+1, Seq.nth toks i)
    else
      error
        { pos = Token.getSource (Seq.nth toks i)
        , what =
            "Unexpected token. Expected to see "
            ^ "'" ^ Token.reservedToString rc ^ "'"
        , explain = NONE
        }


  fun maybeReserved toks rc i =
    if isReserved toks rc i then
      (i+1, SOME (Seq.nth toks i))
    else
      (i, NONE)


  fun tyvar toks i =
    if check toks Token.isTyVar i then
      (i+1, Seq.nth toks i)
    else
      error
        { pos = Token.getSource (Seq.nth toks i)
        , what = "Expected tyvar."
        , explain = NONE
        }


  fun sigid toks i =
    if check toks Token.isStrIdentifier i then
      (i+1, Seq.nth toks i)
    else
      error
        { pos = Token.getSource (Seq.nth toks i)
        , what = "Expected structure or signature identifier."
        , explain = SOME "Must be alphanumeric, and cannot start with a\
                         \ prime (')"
        }


  fun vid toks i =
    if check toks Token.isValueIdentifier i then
      (i+1, Seq.nth toks i)
    else
      error
        { pos = Token.getSource (Seq.nth toks i)
        , what = "Unexpected token. Expected value identifier."
        , explain = NONE
        }


  fun longvid toks i =
    if check toks Token.isMaybeLongIdentifier i then
      (i+1, Ast.MaybeLong.make (Seq.nth toks i))
    else
      error
        { pos = Token.getSource (Seq.nth toks i)
        , what = "Expected (possibly long) value identifier."
        , explain = NONE
        }

  fun recordLabel toks i =
    if check toks Token.isRecordLabel i then
      (i+1, Seq.nth toks i)
    else
      error
        { pos = Token.getSource (Seq.nth toks i)
        , what = "Expected record label."
        , explain = NONE
        }

end
