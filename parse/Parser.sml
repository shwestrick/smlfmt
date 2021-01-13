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


  fun seqFromRevList list = Seq.rev (Seq.fromList list)


  fun makeInfix infdict (left, id, right) =
    let
      val default =
        Ast.Exp.Infix
          { left = left
          , id = id
          , right = right
          }
    in
      case right of
        Ast.Exp.Infix {left=rLeft, id=rId, right=rRight} =>
          if InfixDict.higherPrecedence infdict (rId, id) then
            default
          else
            Ast.Exp.Infix
              { left = makeInfix infdict (left, id, rLeft)
              , id = rId
              , right = rRight
              }
      | _ =>
          default
    end


  (** This just implements a dumb little ordering:
    *   AtExp < AppExp < InfExp < Exp
    * and then e.g. `appExpOkay r` checks `AppExp < r`
    *)
  datatype exp_restrict =
    AtExpRestriction    (* AtExp *)
  | AppExpRestriction   (* AppExp *)
  | InfExpRestriction   (* InfExp *)
  | NoRestriction       (* Exp *)
  fun appExpOkay restrict =
    case restrict of
      AtExpRestriction => false
    | _ => true
  fun infExpOkay restrict =
    case restrict of
      AtExpRestriction => false
    | AppExpRestriction => false
    | _ => true
  fun anyExpOkay restrict =
    case restrict of
      NoRestriction => true
    | _ => false


  fun parse src =
    let
      (** This might raise Lexer.Error *)
      val toksWithComments = Lexer.tokens src
      val toks = Seq.filter (not o Token.isComment) toksWithComments
      val numToks = Seq.length toks
      fun tok i = Seq.nth toks i


      (** not yet implemented *)
      fun nyi fname i =
        if i >= numToks then
          raise Error
            { header = "ERROR: NOT YET IMPLEMENTED"
            , pos = Token.getSource (tok (numToks-1))
            , what = "Unexpected EOF after token."
            , explain = SOME ("(TODO: Sam: see Parser.parse." ^ fname ^ ")")
            }
        else if i >= 0 then
          raise Error
            { header = "ERROR: NOT YET IMPLEMENTED"
            , pos = Token.getSource (tok i)
            , what = "Unexpected token."
            , explain = SOME ("(TODO: Sam: see Parser.parse." ^ fname ^ ")")
            }
        else
          raise Fail ("Bug: Parser.parse." ^ fname ^ ": position out of bounds??")


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


      fun consume_pat infdict i =
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
          consume_patParensOrTupleOrUnit infdict (tok i) [] [] (i+1)
        else
          nyi "consume_pat" i


      (** ( [..., pat,] [pat [, pat ...]] )
        *              ^
        *)
      and consume_patParensOrTupleOrUnit infdict leftParen pats delims i =
        if isReserved Token.CloseParen at i then
          consume_endPatParensOrTupleOrUnit leftParen pats delims (i+1)
        else
          let
            val (i, pat) = consume_pat infdict i
            val pats = pat :: pats
            val (i, delims) =
              if isReserved Token.Comma at i then
                (i+1, tok i :: delims)
              else
                (i, delims)
          in
            consume_patParensOrTupleOrUnit infdict leftParen pats delims i
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
                    , elems = seqFromRevList pats
                    , delims = seqFromRevList delims
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
      fun consume_decMultiple infdict decs delims i =
        if isReserved Token.Val at i then
          let
            val (i, dec) = consume_decVal infdict (i+1)
          in
            consume_maybeContinueDecMultiple infdict (dec :: decs) delims i
          end
        else
          nyi "consume_decMultiple" i


      (** dec [[;] dec ...]
        *     ^
        *)
      and consume_maybeContinueDecMultiple infdict decs delims i =
        let
          val (i, delims) =
            if isReserved Token.Semicolon at i then
              (i+1, SOME (tok i) :: delims)
            else
              (i, delims)
        in
          if check Token.isDecStartToken at i then
            consume_decMultiple infdict decs (NONE :: delims) i
          else if List.length delims = 0 andalso List.length decs = 1 then
            (i, List.hd decs)
          else
            let
              val delims = seqFromRevList delims
              val decs = seqFromRevList decs
            in
              ( i
              , Ast.Exp.DecMultiple
                { elems = decs
                , delims = delims
                }
              )
            end
        end


      and consume_dec infdict i =
        if check Token.isDecStartToken at i then
          consume_decMultiple infdict [] [] i
        else
          (i, Ast.Exp.DecEmpty)


      (** val tyvarseq [rec] pat = exp [and [rec] pat = exp ...]
        *     ^
        *)
      and consume_decVal infdict i =
        let
          val (i, tyvars) = consume_tyvars i
          val (i, recc) = consume_maybeRec i
          val (i, pat) = consume_pat infdict i
          val (i, eq) = consume_expectReserved Token.Equal i
          val (i, exp) = consume_exp infdict NoRestriction i
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


      and consume_exp infdict restriction i =
        let
          val (i, exp) =
            if check Token.isConstant at i then
              (i+1, Ast.Exp.Const (tok i))
            else if isReserved Token.OpenParen at i then
              consume_expParensOrTupleOrUnitOrSequence infdict (tok i) [] [] (i+1)
            else if isReserved Token.Let at i then
              consume_expLetInEnd infdict (i+1)
            else if check Token.isMaybeLongIdentifier at i then
              ( i+1
              , Ast.Exp.Ident
                  { opp = NONE
                  , id = Ast.MaybeLong.make (tok i)
                  }
              )
            else
              nyi "consume_exp" i
        in
          consume_afterExp infdict restriction exp i
        end


      (** exp ...
        *    ^
        *
        * Multiple possibilities for what could be found after an expression:
        *   exp : ty              -- type annotation
        *   exp handle ...        -- handle exception
        *   infexp vid infexp     -- infix application
        *   appexp atexp          -- application
        *   exp andalso exp
        *   exp orelse exp
        *
        * Or, to definitely pop back up, we might see
        *   exp )            -- end of parens, tuple, etc.
        *   exp ,            -- continue tuple
        *   exp ;            -- continue sequence
        *   exp |            -- next in match
        *   exp (then|else)  -- if ... then ... else
        *   exp do           -- while ... do ...
        *   exp of           -- case ... of
        *)
      and consume_afterExp infdict restriction exp i =
        let
          val (again, (i, exp)) =
            if
              i >= numToks orelse
              check Token.endsCurrentExp at i
            then
              (false, (i, exp))

            else if
              anyExpOkay restriction
              andalso isReserved Token.Colon at i
            then
              (true, consume_expTyped exp (i+1))

            else if
              anyExpOkay restriction
              andalso isReserved Token.Handle at i
            then
              (true, consume_expHandle infdict exp (i+1))

            else if
              anyExpOkay restriction
              andalso (isReserved Token.Andalso at i
              orelse isReserved Token.Orelse at i)
            then
              (true, consume_expAndalsoOrOrelse infdict exp (i+1))

            else if
              infExpOkay restriction
              andalso Ast.Exp.isInfExp exp
              andalso check Token.isValueIdentifier at i
              andalso InfixDict.contains infdict (tok i)
            then
              (true, consume_expInfix infdict exp (i+1))

            else if
              appExpOkay restriction
            then
              (true, consume_expApp infdict exp i)

            else
              (false, (i, exp))
        in
          if again then
            consume_afterExp infdict restriction exp i
          else
            (i, exp)
        end



      (** infexp1 vid infexp2
        *            ^
        *)
      and consume_expInfix infdict exp1 i =
        let
          (* val _ = print ("infix\n") *)

          val id = tok (i-1)
          val (i, exp2) = consume_exp infdict InfExpRestriction i
        in
          ( i
          , makeInfix infdict (exp1, id, exp2)
          )
        end



      (** appexp atexp
        *       ^
        *)
      and consume_expApp infdict leftExp i =
        let
          (* val _ = print ("app\n") *)

          val (i, rightExp) = consume_exp infdict AtExpRestriction i
        in
          ( i
          , Ast.Exp.App
              { left = leftExp
              , right = rightExp
              }
          )
        end


      (** exp handle ...
        *           ^
        *)
      and consume_expHandle infdict exp i =
        nyi "consume_expHandle" (i-1)



      (** exp1 (andalso|orelse) exp2
        *                      ^
        *)
      and consume_expAndalsoOrOrelse infdict exp1 i =
        let
          val junct = tok (i-1)
          val (i, exp2) = consume_exp infdict NoRestriction i

          val result =
            if Token.isAndalso junct then
              Ast.Exp.Andalso
                { left = exp1
                , andalsoo = junct
                , right = exp2
                }
            else if Token.isOrelse junct then
              Ast.Exp.Orelse
                { left = exp1
                , orelsee = junct
                , right = exp2
                }
            else
              raise Fail "Bug: Parser.parse.consume_expAndalsoOrOrelse"
        in
          (i, result)
        end



      (** exp : ty
        *      ^
        *)
      and consume_expTyped exp i =
        let
          (* val _ = print ("typed\n") *)

          val colon = tok (i-1)
          val (i, ty) = consume_ty i
        in
          ( i
          , Ast.Exp.Typed
              { exp = exp
              , colon = colon
              , ty = ty
              }
          )
        end



      and consume_ty i =
        let
          val (i, ty) =
            if check Token.isTyVar at i then
              ( i+1
              , Ast.Ty.Var (tok i)
              )
            else if isReserved Token.OpenParen at i then
              let
                val leftParen = tok i
                val (i, ty) = consume_ty (i+1)
              in
                consume_tyParensOrSequence leftParen [ty] [] i
              end
            else if check Token.isMaybeLongIdentifier at i then
              ( i+1
              , Ast.Ty.Con
                  { id = Ast.MaybeLong.make (tok i)
                  , args = Ast.SyntaxSeq.Empty
                  }
              )
            else
              nyi "consume_ty" i
        in
          consume_afterTy ty i
        end


      (** ty
        *   ^
        *
        * Multiple possibilities for what could be found after a type:
        *   ty -> ty        -- function type
        *   ty longtycon    -- type constructor
        *)
      and consume_afterTy ty i =
        let
          val (again, (i, ty)) =
            if check Token.isMaybeLongIdentifier at i then
              ( true
              , ( i+1
                , Ast.Ty.Con
                    { id = Ast.MaybeLong.make (tok i)
                    , args = Ast.SyntaxSeq.One ty
                    }
                )
              )
            else if isReserved Token.Arrow at i then
              (true, consume_tyArrow ty (i+1))
            else
              (false, (i, ty))
        in
          if again then
            consume_afterTy ty i
          else
            (i, ty)
        end



      (** ty -> ty
        *      ^
        *)
      and consume_tyArrow fromTy i =
        let
          val arrow = tok (i-1)
          val (i, toTy) = consume_ty i
        in
          ( i
          , Ast.Ty.Arrow
              { from = fromTy
              , arrow = arrow
              , to = toTy
              }
          )
        end


      (** ( ty )
        *     ^
        * OR
        * ( ty [, ty ...] ) longtycon
        *     ^
        *)
      and consume_tyParensOrSequence leftParen tys delims i =
        if isReserved Token.CloseParen at i then
          consume_tyEndParensOrSequence leftParen tys delims (i+1)
        else if isReserved Token.Comma at i then
          let
            val comma = tok i
            val (i, ty) = consume_ty (i+1)
          in
            consume_tyParensOrSequence leftParen (ty :: tys) (comma :: delims) i
          end
        else
          error
            { pos = Token.getSource (tok i)
            , what = "Unexpected token."
            , explain = NONE
            }



      (** ( ty )
        *       ^
        * OR
        * ( ty, ... ) longtycon
        *            ^
        *)
      and consume_tyEndParensOrSequence leftParen tys delims i =
        let
          val rightParen = tok (i-1)
        in
          case (tys, delims) of
            ([ty], []) =>
              ( i
              , Ast.Ty.Parens
                  { left = leftParen
                  , ty = ty
                  , right = rightParen
                  }
              )

          | _ =>
              (** TODO: BUG:
                *   isMaybeLongIdentifier is not enough. We need to check that
                *   this is a valid tycon. For example, it should not be a
                *   type var.
                *)
              if check Token.isMaybeLongIdentifier at i then
                ( i+1
                , Ast.Ty.Con
                    { id = Ast.MaybeLong.make (tok i)
                    , args =
                        Ast.SyntaxSeq.Many
                          { left = leftParen
                          , elems = seqFromRevList tys
                          , delims = seqFromRevList delims
                          , right = rightParen
                          }
                    }
                )
              else
                error
                  { pos = Token.getSource (tok i)
                  , what = "Unexpected token."
                  , explain = SOME "Expected to see a type constructor."
                  }
        end



      (** let dec in exp [; exp ...] end
        *    ^
        *)
      and consume_expLetInEnd infdict i =
        let
          val lett = tok (i-1)
          val (i, dec) = consume_dec infdict i
          val (i, inn) = consume_expectReserved Token.In i
          val (i, exp) = consume_exp infdict NoRestriction i
        in
          consume_expLetInEndSequence infdict lett dec inn [exp] [] i
        end


      (** let dec in exp [; exp ...] end
        *               ^
        *)
      and consume_expLetInEndSequence infdict lett dec inn exps delims i =
        if isReserved Token.Semicolon at i then
          let
            val delim = tok i
            val (i, exp) = consume_exp infdict NoRestriction (i+1)
          in
            consume_expLetInEndSequence
              infdict
              lett dec inn
              (exp :: exps)
              (delim :: delims)
              i
          end
        else if isReserved Token.End at i then
          if List.length delims <> List.length exps - 1 then
            raise Fail "Bug: Parser.parse.consume_expLetInEndSequence"
          else
            ( i+1
            , Ast.Exp.LetInEnd
                { lett = lett
                , dec = dec
                , inn = inn
                , exps = seqFromRevList exps
                , delims = seqFromRevList delims
                , endd = tok i
                }
            )
        else
          error
            { pos = Token.getSource (tok i)
            , what = "Expected either a semicolon or an 'end'."
            , explain = NONE
            }


      (** ( [...; exp;] [exp [; exp ...]] )
        *              ^
        * OR
        * ( [..., exp,] [exp [, exp ...]] )
        *              ^
        *)
      and consume_expParensOrTupleOrUnitOrSequence infdict leftParen exps delims i =
        if isReserved Token.CloseParen at i then
          consume_endExpParensOrTupleOrUnitOrSequence leftParen exps delims (i+1)
        else
          let
            val (i, exp) = consume_exp infdict NoRestriction i
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
            consume_expParensOrTupleOrUnitOrSequence infdict leftParen exps delims i
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
                    , elems = seqFromRevList exps
                    , delims = seqFromRevList delims
                    , right = rightParen
                    }
                else if Token.isSemicolon exampleDelim then
                  Ast.Exp.Sequence
                    { left = leftParen
                    , elems = seqFromRevList exps
                    , delims = seqFromRevList delims
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


      val infdict = InfixDict.initialTopLevel
      val (i, topdec) = consume_dec infdict 0

      val _ =
        print ("Successfully parsed "
               ^ Int.toString i ^ " out of " ^ Int.toString numToks
               ^ " tokens\n")
    in
      Ast.Dec topdec
    end


end
