(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure Parser:
sig
  exception Error of LineError.t
  val parse: Source.t -> Ast.t
end =
struct

  exception Error of LineError.t

  fun error errSpec =
    raise Error errSpec

  fun parse src =
    let
      (** This might raise Lexer.Error *)
      val toks = Lexer.tokens src
      val numToks = Seq.length toks
      fun tok i = Seq.nth toks i


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


      fun consume_tyvarsMany i (args as {start}) =
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
          consume_tyvarsMany (i+2) args
        else
          error
            { pos = Token.getSource (tok i)
            , what = "Invalid type-variable sequence"
            , explain = "Expected to see something that looks like ('a, 'b, 'c)"
            }


      fun consume_tyvars i =
        if check isTyVar at i then
          (i+1, Ast.SyntaxSeq.One (tok i))
        else if isReserved Token.OpenParen at i andalso check isTyVar at (i+1) then
          consume_tyvarsMany (i+2) {start = i}
        else
          (i, Ast.SyntaxSeq.Empty)


      fun consume_maybeRec i =
        if isReserved Token.Rec at i then
          (i+1, SOME (tok i))
        else
          (i, NONE)


      fun consume_pat i =
        if isReserved Token.Underscore at i then
          ( i+1
          , Ast.Pat.Atpat (Ast.Pat.Wild (tok i))
          )
        else if check Token.isPatternConstant at i then
          ( i+1
          , Ast.Pat.Atpat (Ast.Pat.Const (tok i))
          )
        else if isReserved Token.Op at i then
          (** TODO *)
          error
            { pos = Token.getSource (tok i)
            , what = "Not implemented yet."
            , explain = NONE
            }
        else if isReserved Token.OpenParen at i then
          let
            val (i', pat) = consume_pat (i+1)
          in
            if isReserved Token.CloseParen at i' then
              ( i'+1
              , Ast.Pat.Parens
                  { left = tok i
                  , pat = pat
                  , right = tok i'
                  }
              )
            else
              error
                { pos = Token.getSource (tok i)
                , what = "Unmatched parentheses in pattern."
                , explain = NONE
                }
          end


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
              error
                { pos = Token.getSource (tok i)
                , what = "Unexpected token (not implemented yet)"
                , explain = NONE
                }


      (** val tyvarseq [rec] pat = exp [and [rec] pat = exp ...]
        *     ^
        *)
      and loop_decVal acc i =
        let
          val (i, tyvars) = consume_tyvars i
          val (i, recc) = consume_maybeRec i
          val (i, pat) = consume_pat i
        in
        end

    in
    end


end
