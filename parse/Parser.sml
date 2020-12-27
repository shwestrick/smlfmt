(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure Parser:
sig
  val parse: Source.t -> (Ast.t, LineError.t) MaybeError.t
end =
struct

(*
  fun parse toks =
    let
      val numToks = Seq.length toks
      fun tok i = Seq.nth toks i

      fun success acc =
        ParseResult.Success acc

      fun error acc errSpec =
        ParseResult.Failure
          { partial = acc
          , error = ParseResult.Error errSpec
          }


      (** This silliness lets you write almost-English like this:
        *   if is Token.Identifier at i           then ...
        *   if isReserved Token.Val at i          then ...
        *   if check isTyVar at i                 then ...
        *)
      infix 5 at
      fun f at i = f i
      fun check f i = i < numToks andalso f (tok i)
      fun is c = check (fn t => c = Token.getClass t)
      fun isReserved rc = check (fn t => Token.Reserved rc = Token.getClass t)


      fun consume_tyvarSeqMany i (args as {start}) =
        if isReserved Token.CloseParen at i then
          let
            val numElems = (i-start) div 2
            fun elem j = tok (start + 1 + 2*j)
            fun delim j = tok (start + 2 + 2*j)
            val result =
              Ast.SyntaxSeq.Many
                { left = tok start
                , right = tok i
                , elems = Seq.tabulate elem numElems
                , delims = Seq.tabulate delim (numElems-1)
                }
          in
            (i+1, result)
          end
        else if isReserved Token.Comma at i andalso check isTyVar at (i+1) then
          consume_tyvarSeqMany (i+2) args
        else
          (** TODO: need to restructure in terms of some sort of error Monad?? *)
          raise Fail "Error: consume_tyvarSeqMany"


      fun consume_tyvars i =
        if isReserved Token.OpenParen at i andalso check isTyVar at (i+1) then
          consume_tyvarSeqMany (i+2) {start = i}


      fun loop_topLevel acc i =
        if i >= numToks then
          (** DONE *)
          success acc
        else
          case Token.getClass (tok i) of
            Token.Comment =>
              loop_topLevel acc (i+1)

          | Token.Reserved Token.Val =>
              loop_decVal acc (i+1)

          | _ =>
              error acc
                { pos = Token.getSource (tok i)
                , what = "Unexpected token (not implemented yet)"
                , explain = NONE
                }


      (** val tyvarseq [rec] pat = exp [and [rec] pat = exp ...]
        *     ^
        *)
      and loop_decVal acc i =
        if isReserved Token.OpenParen at i andalso check isTyVar at (i+1) then
        let
          val (i, tyvars) = consume_tyvars i
        in
        end
    in
    end
*)

  fun parse src =
    let
      fun nextFrom i =
        Lexer.next (Source.drop src i)

      fun tokEndOffset tok =
        Source.absoluteEndOffset (Token.getSource tok)

      fun loop_topLevel acc i =
        let
          val tok = nextFromOffset i
          val i' = tokEndOffset tok
        in
          case Token.getClass tok of
            Token.Comment =>
              loop_topLevel acc i'

          | Token.Reserved Token.Val =>
              loop_decVal acc i'

          | _ =>
              error acc
                { pos = Token.getSource tok
                , what = "Unexpected token (not implemented yet)"
                , explain = NONE
                }
        end

      and loop_decVal =

    in
    end

end
