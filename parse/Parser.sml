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
      val hp = InfixDict.higherPrecedence infdict
      val sp = InfixDict.samePrecedence infdict
      val aLeft = InfixDict.associatesLeft infdict
      val aRight = InfixDict.associatesRight infdict

      fun bothLeft (x, y) = aLeft x andalso aLeft y
      fun bothRight (x, y) = aRight x andalso aRight y

      val default =
        Ast.Exp.Infix
          { left = left
          , id = id
          , right = right
          }
    in
      case right of
        Ast.Exp.Infix {left=rLeft, id=rId, right=rRight} =>
          if hp (rId, id) orelse (sp (rId, id) andalso bothRight (rId, id)) then
            default
          else if hp (id, rId) orelse (sp (rId, id) andalso bothLeft (rId, id)) then
            Ast.Exp.Infix
              { left = makeInfix infdict (left, id, rLeft)
              , id = rId
              , right = rRight
              }
          else
            error
              { pos = Token.getSource rId
              , what = "Ambiguous infix expression."
              , explain =
                  SOME "You are not allowed to mix left- and right-associative \
                       \operators of same precedence"
              }

      | _ =>
          default
    end


  fun updateInfixDict infdict dec =
    let
      fun parsePrec p =
        case p of
          NONE => 0
        | SOME x =>
            case Int.fromString (Token.toString x) of
              SOME y => y
            | NONE => raise Fail "Bug: updateInfixDict.parsePrec"
    in
      case dec of
        Ast.Exp.DecInfix {precedence, elems, ...} =>
          let
            val p = parsePrec precedence
            fun mk tok = (tok, p, InfixDict.AssocLeft)
          in
            Seq.iterate (fn (d, tok) => InfixDict.insert d (mk tok))
              infdict
              elems
          end

      | Ast.Exp.DecInfixr {precedence, elems, ...} =>
          let
            val p = parsePrec precedence
            fun mk tok = (tok, p, InfixDict.AssocRight)
          in
            Seq.iterate (fn (d, tok) => InfixDict.insert d (mk tok))
              infdict
              elems
          end

      | Ast.Exp.DecNonfix {elems, ...} =>
          Seq.iterate (fn (d, tok) => InfixDict.remove d tok)
            infdict
            elems

      | _ =>
        raise Fail "Bug: Parser.updateInfixDict: argument is not an infixity dec"
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

      fun consume_maybeReserved rc i =
        if isReserved rc at i then
          (i+1, SOME (tok i))
        else
          (i, NONE)

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


      fun check_normalOrOpInfix infdict opp vid =
        if InfixDict.contains infdict vid andalso not (Option.isSome opp) then
          error
            { pos = Token.getSource vid
            , what = "Infix identifier not prefaced by 'op'"
            , explain = NONE
            }
        else
          ()


      fun consume_pat {nonAtomicOkay} infdict i =
        if isReserved Token.Underscore at i then
          ( i+1
          , Ast.Pat.Wild (tok i)
          )
        else if check Token.isPatternConstant at i then
          ( i+1
          , Ast.Pat.Const (tok i)
          )
        else if check Token.isMaybeLongIdentifier at i then
          ( i+1
          , Ast.Pat.Ident
              { opp = NONE
              , id = Ast.MaybeLong.make (tok i)
              }
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
            val (i, pat) = consume_pat {nonAtomicOkay=true} infdict i
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
                Ast.Pat.Unit
                  { left = leftParen
                  , right = rightParen
                  }
            | ([pat], []) =>
                Ast.Pat.Parens
                  { left = leftParen
                  , pat = pat
                  , right = rightParen
                  }
            | _ =>
                if not (List.length delims = List.length pats - 1) then
                  raise Fail
                    "Bug: Parser.parse.consume_endPatParensOrTupleOrUnit: \
                    \number of patterns and delimiters don't match."
                else
                  Ast.Pat.Tuple
                    { left = leftParen
                    , elems = seqFromRevList pats
                    , delims = seqFromRevList delims
                    , right = rightParen
                    }
        in
          (i, ast)
        end


      fun consume_dec infdict i =
        if check Token.isDecStartToken at i then
          consume_decMultiple infdict [] [] i
        else
          (i, infdict, Ast.Exp.DecEmpty)


      (** dec [[;] dec ...]
        * ^
        *)
      and consume_decMultiple infdict decs delims i =
        if isReserved Token.Val at i then
          let
            val (i, dec) = consume_decVal infdict (i+1)
          in
            consume_maybeContinueDecMultiple infdict (dec :: decs) delims i
          end
        else if isReserved Token.Type at i then
          let
            val (i, dec) = consume_decType infdict (i+1)
          in
            consume_maybeContinueDecMultiple infdict (dec :: decs) delims i
          end
        else if isReserved Token.Infix at i then
          let
            val (i, dec) = consume_decInfix {isLeft=true} infdict (i+1)
            val infdict = updateInfixDict infdict dec
          in
            consume_maybeContinueDecMultiple infdict (dec :: decs) delims i
          end
        else if isReserved Token.Infixr at i then
          let
            val (i, dec) = consume_decInfix {isLeft=false} infdict (i+1)
            val infdict = updateInfixDict infdict dec
          in
            consume_maybeContinueDecMultiple infdict (dec :: decs) delims i
          end
        else if isReserved Token.Nonfix at i then
          let
            val (i, dec) = consume_decNonfix infdict (i+1)
            val infdict = updateInfixDict infdict dec
          in
            consume_maybeContinueDecMultiple infdict (dec :: decs) delims i
          end
        else if isReserved Token.Fun at i then
          let
            val (i, dec) = consume_decFun infdict (i+1)
          in
            consume_maybeContinueDecMultiple infdict (dec :: decs) delims i
          end
        else
          nyi "consume_decMultiple" i


      (** fun tyvarseq [op]vid atpat ... atpat [: ty] = exp [| ...] [and ...]
        *    ^
        *
        * TODO: implement multiple func definitions separated by '|'s, and
        * mutually recursive definitions separated by 'and's.
        *)
      and consume_decFun infdict i =
        let
          val funn = tok (i-1)
          val (i, tyvars) = consume_tyvars i
          val (i, opp) = consume_maybeReserved Token.Op i
          val (i, vid) =
            if check Token.isValueIdentifier at i then
              (i+1, tok i)
            else
              error
                { pos = Token.getSource (tok i)
                , what = "Unexpected token. Expected identifier"
                , explain = NONE
                }

          val _ = check_normalOrOpInfix infdict opp vid

          fun loop acc i =
            if isReserved Token.Colon at i orelse isReserved Token.Equal at i then
              (i, seqFromRevList acc)
            else
              let
                val (i, atpat) = consume_pat {nonAtomicOkay=false} infdict i
              in
                loop (atpat :: acc) i
              end

          val (i, args) = loop [] i

          val (i, ty) =
            if not (isReserved Token.Colon at i) then
              (i, NONE)
            else
              let
                val colon = tok i
                val (i, ty) = consume_ty {permitArrows=true} (i+1)
              in
                (i, SOME {colon = colon, ty = ty})
              end
          val (i, eq) = consume_expectReserved Token.Equal i
          val (i, exp) = consume_exp infdict NoRestriction i

          val fvalbind =
            { delims = Seq.empty ()  (** 'and' delimiters *)
            , elems = Seq.singleton
                { delims = Seq.empty () (** '|' delimiters *)
                , elems = Seq.singleton
                    { opp = opp
                    , id = vid
                    , args = args
                    , ty = ty
                    , eq = eq
                    , exp = exp
                    }
                }
            }
        in
          ( i
          , Ast.Exp.DecFun
              { funn = funn
              , tyvars = tyvars
              , fvalbind = fvalbind
              }
          )
        end


      (** infix [d] vid [vid ...]
        *      ^
        *)
      and consume_decInfix {isLeft} infdict i =
        let
          val infixx = tok (i-1)

          val (i, precedence) =
            if check Token.isDecimalIntegerConstant at i then
              (i+1, SOME (tok i))
            else
              (i, NONE)

          fun loop acc i =
            if check Token.isValueIdentifier at i then
              loop (tok i :: acc) (i+1)
            else
              (i, seqFromRevList acc)

          val (i, elems) = loop [] i
        in
          if Seq.length elems = 0 then
            error
              { pos = Token.getSource (tok i)
              , what = "Unexpected token. Missing identifier."
              , explain = NONE
              }
          else if isLeft then
            ( i
            , Ast.Exp.DecInfix
                { infixx = infixx
                , precedence = precedence
                , elems = elems
                }
            )
          else
            ( i
            , Ast.Exp.DecInfixr
                { infixrr = infixx
                , precedence = precedence
                , elems = elems
                }
            )
        end


      (** nonfix vid [vid ...]
        *       ^
        *)
      and consume_decNonfix infdict i =
        let
          val nonfixx = tok (i-1)

          fun loop acc i =
            if check Token.isValueIdentifier at i then
              loop (tok i :: acc) (i+1)
            else
              (i, seqFromRevList acc)

          val (i, elems) = loop [] i
        in
          if Seq.length elems = 0 then
            error
              { pos = Token.getSource (tok i)
              , what = "Unexpected token. Missing identifier."
              , explain = NONE
              }
          else
            ( i
            , Ast.Exp.DecNonfix
                { nonfixx = nonfixx
                , elems = elems
                }
            )
        end


      (** dec [[;] dec ...]
        *     ^
        *)
      and consume_maybeContinueDecMultiple infdict decs delims i =
        let
          val (i, delims) =
            if isReserved Token.Semicolon at i then
              (i+1, SOME (tok i) :: delims)
            else
              (i, NONE :: delims)
        in
          if check Token.isDecStartToken at i then
            consume_decMultiple infdict decs delims i
          else if List.length delims = 0 andalso List.length decs = 1 then
            (i, infdict, List.hd decs)
          else
            let
              val delims = seqFromRevList delims
              val decs = seqFromRevList decs
            in
              ( i
              , infdict
              , Ast.Exp.DecMultiple
                { elems = decs
                , delims = delims
                }
              )
            end
        end


      (** type tyvars tycon = ty
        *     ^
        *
        * TODO: implement possible [and type tyvars tycon = ty and ...]
        *)
      and consume_decType infdict i =
        let
          val typee = tok (i-1)
          val (i, tyvars) = consume_tyvars i
          val (i, tycon) =
            if check Token.isTyCon at i then
              (i+1, tok i)
            else
              error
                { pos = Token.getSource (tok i)
                , what = "Unexpected token. Invalid type constructor."
                , explain = NONE
                }

          val (i, eq) = consume_expectReserved Token.Equal i
          val (i, ty) = consume_ty {permitArrows=true} i

          val typbind =
            { delims = Seq.empty ()
            , elems = Seq.singleton
                { tyvars = tyvars
                , tycon = tycon
                , eq = eq
                , ty = ty
                }
            }
        in
          ( i
          , Ast.Exp.DecType
              { typee = typee
              , typbind = typbind
              }
          )
        end


      (** val tyvarseq [rec] pat = exp [and [rec] pat = exp ...]
        *     ^
        *)
      and consume_decVal infdict i =
        let
          val (i, tyvars) = consume_tyvars i
          val (i, recc) = consume_maybeReserved Token.Rec i
          val (i, pat) = consume_pat {nonAtomicOkay=true} infdict i
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
            else if isReserved Token.Op at i then
              consume_expValueIdentifier infdict (SOME (tok i)) (i+1)
            else if check Token.isMaybeLongIdentifier at i then
              consume_expValueIdentifier infdict NONE i

            else if isReserved Token.Raise at i then
              if anyExpOkay restriction then
                consume_expRaise infdict (i+1)
              else
                error
                  { pos = Token.getSource (tok i)
                  , what = "Unexpected raise exception."
                  , explain = SOME "Try using parentheses: (raise ...)"
                  }

            else if isReserved Token.Fn at i then
              if anyExpOkay restriction then
                consume_expFn infdict (i+1)
              else
                error
                  { pos = Token.getSource (tok i)
                  , what = "Unexpected beginning of anonymous function."
                  , explain = SOME "Try using parentheses: (fn ... => ...)"
                  }

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


      (**  pat => exp [| pat => exp ...]
        * ^
        *)
      and consume_match infdict i =
        let
          fun loop elems delims i =
            let
              val (i, pat) = consume_pat {nonAtomicOkay=true} infdict i
              val (i, arrow) = consume_expectReserved Token.FatArrow i
              val (i, exp) = consume_exp infdict NoRestriction i
              val elems = {pat=pat, arrow=arrow, exp=exp} :: elems
            in
              if not (isReserved Token.Bar at i) then
                (i, seqFromRevList elems, seqFromRevList delims)
              else
                loop elems (tok i :: delims) (i+1)
            end

          val (i, elems, delims) = loop [] [] i
        in
          (i, elems, delims)
        end


      (** fn pat => exp [| pat => exp ...]
        *   ^
        *)
      and consume_expFn infdict i =
        let
          val fnn = tok (i-1)
          val (i, elems, delims) = consume_match infdict i
        in
          ( i
          , Ast.Exp.Fn
              { fnn = fnn
              , elems = elems
              , delims = delims
              }
          )
        end


      (** [op] longvid
        *     ^
        *)
      and consume_expValueIdentifier infdict opp i =
        let
          val (i, vid) =
            if check Token.isMaybeLongIdentifier at i then
              (i+1, Ast.MaybeLong.make (tok i))
            else
              error
                { pos = Token.getSource (tok i)
                , what = "Expected value identifier."
                , explain = NONE
                }

          val _ = check_normalOrOpInfix infdict opp (Ast.MaybeLong.getToken vid)
        in
          ( i
          , Ast.Exp.Ident
              { opp = opp
              , id = vid
              }
          )
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


      (** raise exp
        *      ^
        *)
      and consume_expRaise infdict i =
        let
          val raisee = tok (i-1)
          val (i, exp) = consume_exp infdict NoRestriction i

          val result =
            Ast.Exp.Raise
              { raisee = raisee
              , exp = exp
              }

          (** NOTE: this is technically a noop, because `raise` has low enough
            * precedence that the left rotation will never happen. But I like
            * keeping the code here because it's informative.
            *)
          val result = FixExpPrecedence.maybeRotateLeft result
        in
          (i, result)
        end


      (** exp handle ...
        *           ^
        *)
      and consume_expHandle infdict exp i =
        let
          val handlee = tok (i-1)
          val (i, elems, delims) = consume_match infdict i

          val result =
            Ast.Exp.Handle
              { exp = exp
              , handlee = handlee
              , elems = elems
              , delims = delims
              }

          val result = FixExpPrecedence.maybeRotateLeft result
        in
          (i, result)
        end



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

          val result =
            FixExpPrecedence.maybeRotateLeft result
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
          val (i, ty) = consume_ty {permitArrows=true} i
        in
          ( i
          , Ast.Exp.Typed
              { exp = exp
              , colon = colon
              , ty = ty
              }
          )
        end



      and consume_ty restriction i =
        let
          val (i, ty) =
            if check Token.isTyVar at i then
              ( i+1
              , Ast.Ty.Var (tok i)
              )
            else if isReserved Token.OpenParen at i then
              let
                val leftParen = tok i
                val (i, ty) = consume_ty {permitArrows=true} (i+1)
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
          consume_afterTy restriction ty i
        end


      (** ty
        *   ^
        *
        * Multiple possibilities for what could be found after a type:
        *   ty -> ty        -- function type
        *   ty longtycon    -- type constructor
        *   ty * ...        -- tuple
        *)
      and consume_afterTy (restriction as {permitArrows: bool}) ty i =
        let
          val (again, (i, ty)) =
            if check Token.isMaybeLongTyCon at i then
              ( true
              , ( i+1
                , Ast.Ty.Con
                    { id = Ast.MaybeLong.make (tok i)
                    , args = Ast.SyntaxSeq.One ty
                    }
                )
              )
            else if permitArrows andalso isReserved Token.Arrow at i then
              (true, consume_tyArrow ty (i+1))
            else if check Token.isStar at i then
              (true, consume_tyTuple [ty] [] (i+1))
            else
              (false, (i, ty))
        in
          if again then
            consume_afterTy restriction ty i
          else
            (i, ty)
        end



      (** ty -> ty
        *      ^
        *)
      and consume_tyArrow fromTy i =
        let
          val arrow = tok (i-1)
          val (i, toTy) = consume_ty {permitArrows=true} i
        in
          ( i
          , Ast.Ty.Arrow
              { from = fromTy
              , arrow = arrow
              , to = toTy
              }
          )
        end


      (** [... *] ty * ...
        *             ^
        *)
      and consume_tyTuple tys delims i =
        let
          val star = tok (i-1)
          val (i, ty) = consume_ty {permitArrows=false} i
          val tys = ty :: tys
          val delims = star :: delims
        in
          if check Token.isStar at i then
            consume_tyTuple tys delims (i+1)
          else
            ( i
            , Ast.Ty.Tuple
                { elems = seqFromRevList tys
                , delims = seqFromRevList delims
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
            val (i, ty) = consume_ty {permitArrows=true} (i+1)
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
              if check Token.isMaybeLongTyCon at i then
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
          val (i, infdict, dec) = consume_dec infdict i
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
      val (i, _, topdec) = consume_dec infdict 0

      val _ =
        print ("Successfully parsed "
               ^ Int.toString i ^ " out of " ^ Int.toString numToks
               ^ " tokens\n")
    in
      Ast.Dec topdec
    end


end
