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

  fun error {what, pos, explain} =
    raise Error
      { header = "PARSE ERROR"
      , pos = pos
      , what = what
      , explain = explain
      }


  fun parse src =
    let
      (** This might raise Lexer.Error *)
      val toksWithComments = Lexer.tokens src
      val toks = Seq.filter (not o Token.isComment) toksWithComments
      val numToks = Seq.length toks
      fun tok i = Seq.nth toks i


      (** not yet implemented *)
      fun nyi fname i =
        raise Error
          { header = "ERROR: NOT YET IMPLEMENTED"
          , pos = Token.getSource (tok i)
          , what = "Unexpected token."
          , explain = SOME ("(TODO: Sam: see Parser.parse." ^ fname ^ ")")
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
        else if isReserved Token.Comma at i andalso check Token.isTyVar at (i+1) then
          consume_tyvarsMany (i+2) args
        else
          error
            { pos = Token.getSource (tok i)
            , what = "Unexpected token."
            , explain = SOME "Invalid type-variable sequence. Expected to see \
                             \something that looks like ('a, 'b, 'c)"
            }


      fun consume_tyvars i =
        if check Token.isTyVar at i then
          (i+1, Ast.SyntaxSeq.One (tok i))
        else if isReserved Token.OpenParen at i andalso check Token.isTyVar at (i+1) then
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
        else if check Token.isMaybeLongIdentifier at i then
          ( i+1
          , Ast.Pat.Atpat (Ast.Pat.Ident
              { opp = NONE
              , id = Ast.MaybeLong.make (tok i)
              })
          )
        else if isReserved Token.OpenParen at i then
          consume_patParensOrTupleOrUnit (tok i) [] [] (i+1)
        else
          nyi "consume_pat" i


      (** ( [..., pat,] [pat [, pat ...]] )
        *              ^
        *)
      and consume_patParensOrTupleOrUnit leftParen pats delims i =
        if isReserved Token.CloseParen at i then
          consume_endPatParensOrTupleOrUnit leftParen pats delims (i+1)
        else
          let
            val (i, pat) = consume_pat i
            val pats = pat :: pats
            val (i, delims) =
              if isReserved Token.Comma at i then
                (i+1, tok i :: delims)
              else
                (i, delims)
          in
            consume_patParensOrTupleOrUnit leftParen pats delims i
          end


      (** ( [pat [, pat ...]] )
        *                       ^
        * Immediately past the close paren, we need to figure out whether
        * this is a unit, or parens around a pat, or a tuple.
        *)
      and consume_endPatParensOrTupleOrUnit leftParen pats delims i =
        let
          val rightParen = tok (i-1)
          val ast =
            case (pats, delims) of
              ([], []) =>
                Ast.Pat.Atpat (Ast.Pat.Unit
                  { left = leftParen
                  , right = rightParen
                  })
            | ([pat], []) =>
                Ast.Pat.Atpat (Ast.Pat.Parens
                  { left = leftParen
                  , pat = pat
                  , right = rightParen
                  })
            | _ =>
                if not (List.length delims = List.length pats - 1) then
                  raise Fail
                    "Bug: Parser.parse.consume_endPatParensOrTupleOrUnit: \
                    \number of patterns and delimiters don't match."
                else
                  Ast.Pat.Atpat (Ast.Pat.Tuple
                    { left = leftParen
                    , elems = Seq.rev (Seq.fromList pats)
                    , delims = Seq.rev (Seq.fromList delims)
                    , right = rightParen
                    })
        in
          (i, ast)
        end



      fun consume_expectReserved rc i =
        if isReserved rc at i then
          (i+1, tok i)
        else
          error
            { pos = Token.getSource (tok i)
            , what =
                "Unexpected token. Expected to see "
                ^ "'" ^ Token.reservedToString rc ^ "'"
            , explain = NONE
            }


      (** dec [[;] dec ...]
        * ^
        *)
      fun consume_decMultiple decs delims i =
        if isReserved Token.Val at i then
          let
            val (i, dec) = consume_decVal (i+1)
          in
            consume_maybeContinueDecMultiple (dec :: decs) delims i
          end
        else
          nyi "consume_decMultiple" i


      (** dec [[;] dec ...]
        *     ^
        *)
      and consume_maybeContinueDecMultiple decs delims i =
        let
          val (i, delims) =
            if isReserved Token.Semicolon at i then
              (i+1, SOME (tok i) :: delims)
            else
              (i, delims)
        in
          if check Token.isDecStartToken at i then
            consume_decMultiple decs (NONE :: delims) i
          else if List.length delims = 0 andalso List.length decs = 1 then
            (i, List.hd decs)
          else
            let
              val delims = Seq.rev (Seq.fromList delims)
              val decs = Seq.rev (Seq.fromList decs)
            in
              ( i
              , Ast.Exp.DecMultiple
                { elems = decs
                , delims = delims
                }
              )
            end
        end


      and consume_dec i =
        if check Token.isDecStartToken at i then
          consume_decMultiple [] [] i
        else
          (i, Ast.Exp.DecEmpty)


      (** val tyvarseq [rec] pat = exp [and [rec] pat = exp ...]
        *     ^
        *)
      and consume_decVal i =
        let
          val (i, tyvars) = consume_tyvars i
          val (i, recc) = consume_maybeRec i
          val (i, pat) = consume_pat i
          val (i, eq) = consume_expectReserved Token.Equal i
          val (i, exp) = consume_exp i
        in
          ( i
          , Ast.Exp.DecVal
              { vall = tok (i-1)
              , tyvars = tyvars
              , elems = Seq.singleton
                  { recc = recc
                  , pat = pat
                  , eq = eq
                  , exp = exp
                  }
              , delims = Seq.empty ()
              }
          )
        end


      and consume_exp i =
        if check Token.isConstant at i then
          (i+1, Ast.Exp.Const (tok i))
        else if isReserved Token.OpenParen at i then
          consume_expParensOrTupleOrUnitOrSequence (tok i) [] [] (i+1)
        else if isReserved Token.Let at i then
          consume_expLetInEnd (i+1)
        else
          nyi "consume_exp" i


      (** let dec in exp [; exp ...] end
        *    ^
        *)
      and consume_expLetInEnd i =
        let
          val lett = tok (i-1)
          val (i, dec) = consume_dec i
          val (i, inn) = consume_expectReserved Token.In i
          val (i, exp) = consume_exp i
          val (i, endd) = consume_expectReserved Token.End i
        in
          ( i
          , Ast.Exp.LetInEnd
              { lett = lett
              , dec = dec
              , inn = inn
              , exps = Seq.singleton exp
              , delims = Seq.empty ()
              , endd = endd
              }
          )
        end


      (** ( [...; exp;] [exp [; exp ...]] )
        * OR
        * ( [..., exp,] [exp [, exp ...]] )
        *              ^
        *)
      and consume_expParensOrTupleOrUnitOrSequence leftParen exps delims i =
        if isReserved Token.CloseParen at i then
          consume_endExpParensOrTupleOrUnitOrSequence leftParen exps delims (i+1)
        else
          let
            val (i, exp) = consume_exp i
            val exps = exp :: exps

            (** Try to continue by parsing the next delimiter. It needs to
              * match exiting delimiter.
              *)
            val commasOkay =
              case delims of
                [] => true
              | d :: _ => Token.isComma d
            val semicolonsOkay =
              case delims of
                [] => true
              | d :: _ => Token.isSemicolon d

            val (i, delims) =
              if isReserved Token.Comma at i then
                if commasOkay then
                  (i+1, tok i :: delims)
                else
                  error
                    { pos = Token.getSource (tok i)
                    , what = "Unexpected comma."
                    , explain = SOME
                        "Perhaps you meant to use a semicolon. Sequences of \
                        \expressions are separated by semicolons."
                    }
              else if isReserved Token.Semicolon at i then
                if semicolonsOkay then
                  (i+1, tok i :: delims)
                else
                  error
                    { pos = Token.getSource (tok i)
                    , what = "Unexpected semicolon."
                    , explain = SOME
                        "Perhaps you meant to use a comma. Tuples of \
                        \expressions are separated by commas."
                    }
              else
                (i, delims)
          in
            consume_expParensOrTupleOrUnitOrSequence leftParen exps delims i
          end


      (** ( [exp [; exp ...]] )
        * OR
        * ( [exp [, exp ...]] )
        *                       ^
        * Immediately past the close paren, we need to figure out whether
        * this is a unit, or parens around an exp, a tuple, or a sequence.
        *)
      and consume_endExpParensOrTupleOrUnitOrSequence leftParen exps delims i =
        let
          val rightParen = tok (i-1)
          val ast =
            case (exps, delims) of
              ([], []) =>
                Ast.Exp.Unit
                  { left = leftParen
                  , right = rightParen
                  }
            | ([exp], []) =>
                Ast.Exp.Parens
                  { left = leftParen
                  , exp = exp
                  , right = rightParen
                  }
            | (_, exampleDelim :: _) =>
                if not (List.length delims = List.length exps - 1) then
                  raise Fail
                    "Bug: Parser.parse.consume_endExpParensOrTupleOrUnitOrSequence: \
                    \number of patterns and delimiters don't match."
                else if Token.isComma exampleDelim then
                  Ast.Exp.Tuple
                    { left = leftParen
                    , elems = Seq.rev (Seq.fromList exps)
                    , delims = Seq.rev (Seq.fromList delims)
                    , right = rightParen
                    }
                else if Token.isSemicolon exampleDelim then
                  Ast.Exp.Sequence
                    { left = leftParen
                    , elems = Seq.rev (Seq.fromList exps)
                    , delims = Seq.rev (Seq.fromList delims)
                    , right = rightParen
                    }
                else
                  raise Fail
                    "Bug: Parser.parse.consume_endExpParensOrTupleOrUnitOrSequence: \
                    \invalid delimiters."
            | _ =>
                raise Fail
                  "Bug: Parser.parse.consume_endExpParensOrTupleOrUnitOrSequence"

        in
          (i, ast)
        end


      val (i, topdec) = consume_dec 0

      val _ =
        print ("Successfully parsed "
               ^ Int.toString i ^ " out of " ^ Int.toString numToks
               ^ " tokens\n")
    in
      Ast.Dec topdec
    end


end
