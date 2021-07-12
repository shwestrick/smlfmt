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


  (** ========================================================================
    * Handle infix expressions and patterns by rotating AST according to
    * operator precedence.
    *
    * TODO: DRY: makeInfixExp and makeInfixPat are nearly identical.
    *)

  fun makeInfixExp infdict (left, id, right) =
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
              { left = makeInfixExp infdict (left, id, rLeft)
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


  fun makeInfixPat infdict (left, id, right) =
    let
      val hp = InfixDict.higherPrecedence infdict
      val sp = InfixDict.samePrecedence infdict
      val aLeft = InfixDict.associatesLeft infdict
      val aRight = InfixDict.associatesRight infdict

      fun bothLeft (x, y) = aLeft x andalso aLeft y
      fun bothRight (x, y) = aRight x andalso aRight y

      val default =
        Ast.Pat.Infix
          { left = left
          , id = id
          , right = right
          }
    in
      case right of
        Ast.Pat.Infix {left=rLeft, id=rId, right=rRight} =>
          if hp (rId, id) orelse (sp (rId, id) andalso bothRight (rId, id)) then
            default
          else if hp (id, rId) orelse (sp (rId, id) andalso bothLeft (rId, id)) then
            Ast.Pat.Infix
              { left = makeInfixPat infdict (left, id, rLeft)
              , id = rId
              , right = rRight
              }
          else
            error
              { pos = Token.getSource rId
              , what = "Ambiguous infix pattern."
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


  (** ========================================================================
    * Expression/Pattern hierarchy
    *
    * This just implements a dumb little ordering:
    *   AtExp/Pat < AppExp/Pat < InfExp/Pat < Exp/Pat
    * and then e.g. `appOkay r` checks `AppExp < r`
    *)

  datatype exp_restrict =
    AtRestriction    (* AtExp/Pat *)
  | AppRestriction   (* AppExp/Pat *)
  | InfRestriction   (* InfExp/Pat *)
  | NoRestriction    (* Exp *)
  fun appOkay restrict =
    case restrict of
      AtRestriction => false
    | _ => true
  fun infOkay restrict =
    case restrict of
      AtRestriction => false
    | AppRestriction => false
    | _ => true
  fun anyOkay restrict =
    case restrict of
      NoRestriction => true
    | _ => false

  (** ========================================================================
    *)

  type ('state, 'result) parser = 'state -> ('state * 'result)
  type 'state peeker = 'state -> bool


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

      (** parse_reserved:
        *   Token.reserved -> (int, Token.t) parser
        *)
      fun parse_reserved rc i =
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

      (** parse_zeroOrMoreDelimitedByReserved
        *   { parseElem: (int, 'a) parser
        *   , delim: Token.reserved
        *   , shouldStop: int peeker
        *   }
        *   -> (int, {elems: 'a Seq.t, delims: Token.t Seq.t}) parser
        *)
      fun parse_zeroOrMoreDelimitedByReserved
          {parseElem: (int, 'a) parser, delim: Token.reserved, shouldStop}
          i =
        let
          fun loop elems delims i =
            if shouldStop i then
              (i, elems, delims)
            else
              let
                val (i, elem) = parseElem i
                val elems = elem :: elems
              in
                if isReserved delim at i then
                  loop elems (tok i :: delims) (i+1)
                else
                  (i, elems, delims)
              end

          val (i, elems, delims) = loop [] [] i
        in
          ( i
          , { elems = seqFromRevList elems
            , delims = seqFromRevList delims
            }
          )
        end


      (** parse_oneOrMoreDelimitedByReserved
        *   {parseElem: (int, 'a) parser, delim: Token.reserved} ->
        *   (int, {elems: 'a Seq.t, delims: Token.t Seq.t}) parser
        *)
      fun parse_oneOrMoreDelimitedByReserved
          {parseElem: (int, 'a) parser, delim: Token.reserved}
          i =
        let
          fun loop elems delims i =
            let
              val (i, elem) = parseElem i
              val elems = elem :: elems
            in
              if isReserved delim at i then
                loop elems (tok i :: delims) (i+1)
              else
                (i, elems, delims)
            end

          val (i, elems, delims) = loop [] [] i
        in
          ( i
          , { elems = seqFromRevList elems
            , delims = seqFromRevList delims
            }
          )
        end


      (** parse_two:
        *   ('s, 'a) parser * ('s, 'b) parser
        *   -> ('s, ('a * 'b)) parser
        *)
      fun parse_two (p1, p2) state =
        let
          val (state, elem1) = p1 state
          val (state, elem2) = p2 state
        in
          (state, (elem1, elem2))
        end


      (** parse_while:
        *   's peeker -> ('s, 'a) parser -> ('s, 'a Seq.t) parser
        *)
      fun parse_while continue parse state =
        let
          fun loop elems state =
            if not (continue state) then (state, elems) else
            let
              val (state, elem) = parse state
              val elems = elem :: elems
            in
              loop elems state
            end

          val (state, elems) = loop [] state
        in
          (state, seqFromRevList elems)
        end


(*
      (** parse_interleaveWhile
        *   { parseElem: ('s, 'a) parser
        *   , parseDelim: ('s, 'b) parser
        *   , continue: 's peeker
        *   } ->
        *   ('s, {elems: 'a Seq.t, delims: Token.t Seq.t}) parser
        *)
      fun parse_interleaveWhile
          {parseElem: ('s, 'a) parser, parseDelim: ('s, 'b) parser, continue}
          state =
        let
          fun loopElem elems delims state =
            if continue state then (state, elems, delims) else
            let
              val (state, elem) = parseElem state
              val elems = elem :: elems
            in
              loopDelim elems delims state
            end

          and loopDelim elems delims state =
            if continue state then (state, elems, delims) else
            let
              val (state, delim) = parseDelim state
              val delims = delim :: delims
            in
              loopElem elems delims state
            end

          val (state, elems, delims) = loopElem [] [] state
        in
          ( state
          , { elems = seqFromRevList elems
            , delims = seqFromRevList delims
            }
          )
        end
*)

      fun parse_tyvar i =
        if check Token.isTyVar at i then
          (i+1, tok i)
        else
          error
            { pos = Token.getSource (tok i)
            , what = "Expected tyvar."
            , explain = NONE
            }


      fun parse_tyvars i =
        if check Token.isTyVar at i then
          (i+1, Ast.SyntaxSeq.One (tok i))
        else if not (isReserved Token.OpenParen at i
                     andalso check Token.isTyVar at (i+1)) then
          (i, Ast.SyntaxSeq.Empty)
        else
          let
            val (i, openParen) = (i+1, tok i)
            val (i, {elems, delims}) =
              parse_oneOrMoreDelimitedByReserved
                {parseElem = parse_tyvar, delim = Token.Comma}
                i
            val (i, closeParen) =
              parse_reserved Token.CloseParen i
          in
            ( i
            , Ast.SyntaxSeq.Many
                { left = openParen
                , right = closeParen
                , elems = elems
                , delims = delims
                }
            )
          end


      fun consume_maybeReserved rc i =
        if isReserved rc at i then
          (i+1, SOME (tok i))
        else
          (i, NONE)


      fun check_normalOrOpInfix infdict opp vid =
        if InfixDict.contains infdict vid andalso not (Option.isSome opp) then
          error
            { pos = Token.getSource vid
            , what = "Infix identifier not prefaced by 'op'"
            , explain = NONE
            }
        else
          ()


      fun parse_vid i =
        if check Token.isValueIdentifier at i then
          (i+1, tok i)
        else
          error
            { pos = Token.getSource (tok i)
            , what = "Unexpected token. Expected value identifier."
            , explain = NONE
            }


      fun parse_longvid i =
        if check Token.isMaybeLongIdentifier at i then
          (i+1, Ast.MaybeLong.make (tok i))
        else
          error
            { pos = Token.getSource (tok i)
            , what = "Expected (possibly long) value identifier."
            , explain = NONE
            }

      fun parse_recordLabel i =
        if check Token.isRecordLabel at i then
          (i+1, tok i)
        else
          error
            { pos = Token.getSource (tok i)
            , what = "Expected record label."
            , explain = NONE
            }


      fun consume_opvid infdict i =
        let
          val (i, opp) = consume_maybeReserved Token.Op i
          val (i, vid) = parse_vid i
          val _ = check_normalOrOpInfix infdict opp vid
        in
          (i, {opp = opp, vid = vid})
        end


      fun consume_pat infdict restriction i =
        let
          val (i, pat) =
            if isReserved Token.Underscore at i then
              ( i+1
              , Ast.Pat.Wild (tok i)
              )
            else if check Token.isPatternConstant at i then
              ( i+1
              , Ast.Pat.Const (tok i)
              )
            else if check Token.isMaybeLongIdentifier at i then
              consume_patValueIdentifier infdict NONE i
            else if isReserved Token.Op at i then
              consume_patValueIdentifier infdict (SOME (tok i)) (i+1)
            else if isReserved Token.OpenParen at i then
              consume_patParensOrTupleOrUnit infdict (tok i) (i+1)
            else if isReserved Token.OpenSquareBracket at i then
              consume_patListLiteral infdict (tok i) (i+1)
            else if isReserved Token.OpenCurlyBracket at i then
              consume_patRecord infdict (tok i) (i+1)
            else
              nyi "consume_pat" i
        in
          consume_afterPat infdict restriction pat i
        end


      (** pat
        *    ^
        *
        * Multiple possibilities for what could be found after a pattern:
        *   [op]longvid atpat     -- identifiers might actually be constructors
        *   pat vid pat           -- infix constructor pattern
        *   pat : ty              -- type annotation
        *   [op]vid[: ty] as pat  -- layered
        *)
      and consume_afterPat infdict restriction pat i =
        let
          val (again, (i, pat)) =
            if
              (** Annoying edge case with '='... we can use it in an infix
                * expression as an equality predicate, but it is NEVER valid as
                * an infix constructor, because SML forbids rebinding '=' in
                * any program.
                *
                * Note to language designers... this is strange. Why not just
                * use something reasonable like "==" for equality predicate?
                * It makes the language more readable too...
                *)
              infOkay restriction
              andalso check Token.isValueIdentifierNoEqual at i
              andalso InfixDict.contains infdict (tok i)
            then
              (true, consume_patInfix infdict pat (tok i) (i+1))

            else if
              appOkay restriction
              andalso Ast.Pat.okayForConPat pat
              andalso check Token.isAtPatStartToken at i
            then
              (true, consume_patCon infdict (Ast.Pat.unpackForConPat pat) i)

            else if

              isReserved Token.Colon at i
              andalso anyOkay restriction
            then
              (true, consume_patTyped infdict pat (tok i) (i+1))

            else if
              isReserved Token.As at i
              andalso anyOkay restriction
              andalso Ast.Pat.okayForAsPat pat
            then
              (true, consume_patAs infdict (Ast.Pat.unpackForAsPat pat) (tok i) (i+1))

            else
              (false, (i, pat))
        in
          if again then
            consume_afterPat infdict restriction pat i
          else
            (i, pat)
        end


      (** { patrow [, ...] }
        *  ^
        *)
      and consume_patRecord infdict leftBracket i =
        let
          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = consume_patRow infdict, delim = Token.Comma}
              i

          val (i, rightBracket) = parse_reserved Token.CloseCurlyBracket i
        in
          ( i
          , Ast.Pat.Record
              { left = leftBracket
              , elems = elems
              , delims = delims
              , right = rightBracket
              }
          )
        end


      (** A patrow is one of:
        *   ...
        *   lab = pat
        *   vid[: ty] [as pat]
        *)
      and consume_patRow infdict i =
        if isReserved Token.DotDotDot at i then
          if isReserved Token.CloseCurlyBracket at (i+1) then
            (i+1, Ast.Pat.DotDotDot (tok i))
          else
            error
              { pos = Token.getSource (tok i)
              , what = "Unexpected token."
              , explain = SOME "This can only appear at the end of the record."
              }
        else if check Token.isRecordLabel at i
                andalso isReserved Token.Equal at (i+1) then
          let
            val (i, lab) = (i+1, tok i)
            val (i, eq) = (i+1, tok i)
            val (i, pat) = consume_pat infdict NoRestriction i
          in
            ( i
            , Ast.Pat.LabEqPat
                { lab = lab
                , eq = eq
                , pat = pat
                }
            )
          end
        else if check Token.isValueIdentifierNoEqual at i then
          let
            val (i, vid) = (i+1, tok i)
            val (i, ty) =
              if not (isReserved Token.Colon at i) then
                (i, NONE)
              else
                let
                  val (i, colon) = (i+1, tok i)
                  val (i, ty) = consume_ty {permitArrows=true} i
                in
                  (i, SOME {colon=colon, ty=ty})
                end

            val (i, aspat) =
              if not (isReserved Token.As at i) then
                (i, NONE)
              else
                let
                  val (i, ass) = (i+1, tok i)
                  val (i, pat) = consume_pat infdict NoRestriction i
                in
                  (i, SOME {ass=ass, pat=pat})
                end
          in
            ( i
            , Ast.Pat.LabAsPat
                { id = vid
                , ty = ty
                , aspat = aspat
                }
            )
          end
        else
          error
            { pos = Token.getSource (tok i)
            , what = "Invalid token. Expected row of record pattern."
            , explain = NONE
            }


      (** [op]vid[: ty] as pat
        *                 ^
        *)
      and consume_patAs infdict {opp, id, ty} ass i =
        let
          val (i, pat) = consume_pat infdict NoRestriction i
        in
          ( i
          , Ast.Pat.Layered
              { opp = opp
              , id = id
              , ty = ty
              , ass = ass
              , pat = pat
              }
          )
        end


      (** [op]longvid atpat
        *            ^
        *)
      and consume_patCon infdict {opp, id} i =
        let
          val (i, atpat) = consume_pat infdict AtRestriction i
        in
          ( i
          , Ast.Pat.Con
              { opp = opp
              , id = id
              , atpat = atpat
              }
          )
        end


      (** pat : ty
        *      ^
        *)
      and consume_patTyped infdict pat colon i =
        let
          val (i, ty) = consume_ty {permitArrows=true} i
        in
          ( i
          , Ast.Pat.Typed
              { pat = pat
              , colon = colon
              , ty = ty
              }
          )
        end


      (** pat vid pat
        *        ^
        *)
      and consume_patInfix infdict leftPat vid i =
        let
          val (i, rightPat) = consume_pat infdict InfRestriction i
        in
          ( i
          , makeInfixPat infdict (leftPat, vid, rightPat)
          )
        end


      (** [op] longvid
        *     ^
        *)
      and consume_patValueIdentifier infdict opp i =
        let
          val (i, vid) = parse_longvid i
          val _ = check_normalOrOpInfix infdict opp (Ast.MaybeLong.getToken vid)
        in
          ( i
          , Ast.Pat.Ident
              { opp = opp
              , id = vid
              }
          )
        end


      (** [ ... ]
        *  ^
        *)
      and consume_patListLiteral infdict openBracket i =
        let
          fun finish elems delims closeBracket =
            Ast.Pat.List
              { left = openBracket
              , right = closeBracket
              , elems = elems
              , delims = delims
              }
        in
          if isReserved Token.CloseSquareBracket at i then
            (i+1, finish (Seq.empty ()) (Seq.empty ()) (tok i))
          else
            let
              val parseElem = consume_pat infdict NoRestriction
              val (i, {elems, delims}) =
                parse_oneOrMoreDelimitedByReserved
                  {parseElem = parseElem, delim = Token.Comma}
                  i
              val (i, closeBracket) =
                parse_reserved Token.CloseSquareBracket i
            in
              (i, finish elems delims closeBracket)
            end
        end


      (** ( )
        *  ^
        * OR
        * ( pat )
        *  ^
        * OR
        * ( pat, pat [, pat ...] )
        *  ^
        *)
      and consume_patParensOrTupleOrUnit infdict leftParen i =
        if isReserved Token.CloseParen at i then
          ( i+1
          , Ast.Pat.Unit
              { left = leftParen
              , right = tok i
              }
          )
        else
          let
            val parseElem = consume_pat infdict NoRestriction
            val (i, {elems, delims}) =
              parse_oneOrMoreDelimitedByReserved
                {parseElem = parseElem, delim = Token.Comma}
                i
            val (i, rightParen) = parse_reserved Token.CloseParen i
            val result =
              if Seq.length elems = 1 then
                Ast.Pat.Parens
                  { left = leftParen
                  , pat = Seq.nth elems 0
                  , right = rightParen
                  }
              else
                Ast.Pat.Tuple
                  { left = leftParen
                  , elems = elems
                  , delims = delims
                  , right = rightParen
                  }
          in
            (i, result)
          end


      and consume_dec infdict i =
        let
          fun consume_maybeSemicolon (i, infdict) =
            if isReserved Token.Semicolon at i then
              ((i+1, infdict), SOME (tok i))
            else
              ((i, infdict), NONE)

          (** While we see a dec-start token, parse pairs of
            *   (dec, semicolon option)
            * The "state" in this computation is a pair (i, infdict), because
            * declarations can affect local infixity.
            *)
          val ((i, infdict), decs) =
            parse_while
              (fn (i, _) => check Token.isDecStartToken at i)
              (parse_two (consume_oneDec, consume_maybeSemicolon))
              (i, infdict)

          fun makeDecMultiple () =
            Ast.Exp.DecMultiple
              { elems = Seq.map #1 decs
              , delims = Seq.map #2 decs
              }

          val result =
            case Seq.length decs of
              0 =>
                Ast.Exp.DecEmpty
            | 1 =>
                let
                  val (dec, semicolon) = Seq.nth decs 0
                in
                  if isSome semicolon then
                    makeDecMultiple ()
                  else
                    dec
                end
            | _ =>
                makeDecMultiple ()
        in
          (i, infdict, result)
        end


      and consume_oneDec (i, infdict) =
        if isReserved Token.Val at i then
          consume_decVal (i+1, infdict)
        else if isReserved Token.Type at i then
          consume_decType (i+1, infdict)
        else if isReserved Token.Infix at i then
          consume_decInfix {isLeft=true} (i+1, infdict)
        else if isReserved Token.Infixr at i then
          consume_decInfix {isLeft=false} (i+1, infdict)
        else if isReserved Token.Nonfix at i then
          consume_decNonfix (i+1, infdict)
        else if isReserved Token.Fun at i then
          consume_decFun (i+1, infdict)
        else if isReserved Token.Exception at i then
          consume_decException (tok i) (i+1, infdict)
        else if isReserved Token.Local at i then
          consume_decLocal (i+1, infdict)
        else
          nyi "consume_oneDec" i


      (** exception exbind
        *          ^
        *)
      and consume_decException exceptionn (i, infdict) =
        let
          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = consume_decExbind infdict, delim = Token.And}
              i
        in
          ( (i, infdict)
          , Ast.Exp.DecException
              { exceptionn = exceptionn
              , elems = elems
              , delims = delims
              }
          )
        end


      (**  [op] vid [of ty]
        * ^
        *
        * OR
        *
        *  [op] vid = [op] longvid
        * ^
        *)
      and consume_decExbind infdict i =
        let
          val (i, opp) = consume_maybeReserved Token.Op i
          val (i, vid) = parse_vid i
        in
          if isReserved Token.Of at i then
            let
              val (i, off) = (i+1, tok i)
              val (i, ty) = consume_ty {permitArrows=true} i
            in
              ( i
              , Ast.Exp.ExnNew
                  { opp = opp
                  , id = vid
                  , arg = SOME {off = off, ty = ty}
                  }
              )
            end

          else if isReserved Token.Equal at i then
            let
              val (i, eq) = (i+1, tok i)
              val (i, opp) = consume_maybeReserved Token.Op i
              val (i, longvid) = parse_longvid i
            in
              ( i
              , Ast.Exp.ExnReplicate
                  { opp = opp
                  , left_id = vid
                  , eq = eq
                  , right_id = longvid
                  }
              )
            end

          else
            ( i
            , Ast.Exp.ExnNew
                { opp = opp
                , id = vid
                , arg = NONE
                }
            )
        end


      (** fun tyvarseq [op]vid atpat ... atpat [: ty] = exp [| ...] [and ...]
        *    ^
        *)
      and consume_decFun (i, infdict) =
        let
          (** [op]vid atpat .... atpat [: ty] = exp [| ...] *)
          fun parseElem i =
            let
              val (_, {vid = func_name, ...}) = consume_opvid infdict i

              (** [op]vid atpat ... atpat [: ty] = exp *)
              fun parseBranch vid i =
                let
                  val (i, {opp, vid}) = consume_opvid infdict i

                  (** arg patterns continue until we see
                    * ':' (type annotation) or
                    * '=' (end of args, beginning of function body)
                    *)
                  val (i, args) =
                    parse_while
                      (fn i => not (isReserved Token.Colon at i orelse isReserved Token.Equal at i))
                      (consume_pat infdict AtRestriction)
                      i

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
                  val (i, eq) = parse_reserved Token.Equal i
                  val (i, exp) = consume_exp infdict NoRestriction i
                in
                  if not (Token.same (func_name, vid)) then
                    error
                      { pos = Token.getSource vid
                      , what = "Function name does not match."
                      , explain = SOME ("Expected identifier `" ^ Token.toString
                                  func_name ^ "`.")
                      }
                  else
                    ( i
                    , { opp = opp
                      , id = vid
                      , args = args
                      , ty = ty
                      , eq = eq
                      , exp = exp
                      }
                    )
                end
              val (i, func_def) =
                parse_oneOrMoreDelimitedByReserved
                  {parseElem = parseBranch func_name, delim = Token.Bar}
                  i
            in
              (i, func_def)
            end

          val funn = tok (i-1)
          val (i, tyvars) = parse_tyvars i
          val (i, fvalbind) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseElem, delim = Token.And}
              i
        in
          ( (i, infdict)
          , Ast.Exp.DecFun
              { funn = funn
              , tyvars = tyvars
              , fvalbind = fvalbind
              }
          )
        end

      (** local dec1 in dec2 end
        *      ^
        *)
      and consume_decLocal (i, infdict) =
        let
          val original_infdict = infdict

          val locall = tok (i-1)
          val (i, infdict, dec1) = consume_dec infdict i
          val (i, inn) = parse_reserved Token.In i
          val (i, _, dec2) = consume_dec infdict i
          val (i, endd) = parse_reserved Token.End i
        in
          ( (i, original_infdict)
          , Ast.Exp.DecLocal
              { locall = locall
              , left_dec = dec1
              , inn = inn
              , right_dec = dec2
              , endd = endd
              }
          )
        end


      (** infix [d] vid [vid ...]
        *      ^
        *)
      and consume_decInfix {isLeft} (i, infdict) =
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

          val result =
            if Seq.length elems = 0 then
              error
                { pos = Token.getSource (tok i)
                , what = "Unexpected token. Missing identifier."
                , explain = NONE
                }
            else if isLeft then
              Ast.Exp.DecInfix
                { infixx = infixx
                , precedence = precedence
                , elems = elems
                }
            else
              Ast.Exp.DecInfixr
                { infixrr = infixx
                , precedence = precedence
                , elems = elems
                }
        in
          ((i, updateInfixDict infdict result), result)
        end


      (** nonfix vid [vid ...]
        *       ^
        *)
      and consume_decNonfix (i, infdict) =
        let
          val nonfixx = tok (i-1)

          fun loop acc i =
            if check Token.isValueIdentifier at i then
              loop (tok i :: acc) (i+1)
            else
              (i, seqFromRevList acc)

          val (i, elems) = loop [] i

          val result =
            if Seq.length elems = 0 then
              error
                { pos = Token.getSource (tok i)
                , what = "Unexpected token. Missing identifier."
                , explain = NONE
                }
            else
              Ast.Exp.DecNonfix
                { nonfixx = nonfixx
                , elems = elems
                }
        in
          ((i, updateInfixDict infdict result), result)
        end


      (** type tyvars tycon = ty [and tyvars tycon = ty ...]
        *     ^
        *)
      and consume_decType (i, infdict) =
        let
          val typee = tok (i-1)

          fun parseElem i =
            let
              val (i, tyvars) = parse_tyvars i
              val (i, tycon) =
                if check Token.isTyCon at i then
                  (i+1, tok i)
                else
                  error
                    { pos = Token.getSource (tok i)
                    , what = "Unexpected token. Invalid type constructor."
                    , explain = NONE
                    }

              val (i, eq) = parse_reserved Token.Equal i
              val (i, ty) = consume_ty {permitArrows=true} i
            in
              ( i
              , { tyvars = tyvars
                , tycon = tycon
                , eq = eq
                , ty = ty
                }
              )
            end

          val (i, typbind) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseElem, delim = Token.And}
              i
        in
          ( (i, infdict)
          , Ast.Exp.DecType
              { typee = typee
              , typbind = typbind
              }
          )
        end


      (** val tyvarseq [rec] pat = exp [and [rec] pat = exp ...]
        *     ^
        *)
      and consume_decVal (i, infdict) =
        let
          (**  [rec] pat = exp
            * ^
            *)
          fun parseElem i =
            let
              val (i, recc) = consume_maybeReserved Token.Rec i
              val (i, pat) = consume_pat infdict NoRestriction i
              val (i, eq) = parse_reserved Token.Equal i
              val (i, exp) = consume_exp infdict NoRestriction i
            in
              ( i
              , { recc = recc
                , pat = pat
                , eq = eq
                , exp = exp
                }
              )
            end

          val vall = tok (i-1)
          val (i, tyvars) = parse_tyvars i
          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseElem, delim = Token.And}
              i
        in
          ( (i, infdict)
          , Ast.Exp.DecVal
              { vall = vall
              , tyvars = tyvars
              , elems = elems
              , delims = delims
              }
          )
        end


      and consume_exp infdict restriction i =
        let
          val (i, exp) =
            if check Token.isConstant at i then
              (i+1, Ast.Exp.Const (tok i))
            else if isReserved Token.OpenParen at i then
              consume_expParensOrTupleOrUnitOrSequence infdict (tok i) (i+1)
            else if isReserved Token.OpenSquareBracket at i then
              consume_expListLiteral infdict (i+1)
            else if isReserved Token.OpenCurlyBracket at i then
              consume_expRecord infdict (tok i) (i+1)
            else if isReserved Token.Let at i then
              consume_expLetInEnd infdict (i+1)
            else if isReserved Token.Op at i then
              consume_expValueIdentifier infdict (SOME (tok i)) (i+1)
            else if check Token.isMaybeLongIdentifier at i then
              consume_expValueIdentifier infdict NONE i
            else if isReserved Token.Case at i then
              consume_expCase infdict (i+1)
            else if isReserved Token.Hash at i then
              consume_expSelect (tok i) (i+1)
            else if isReserved Token.If at i then
              if anyOkay restriction then
                consume_expIfThenElse infdict (tok i) (i+1)
              else
                error
                  { pos = Token.getSource (tok i)
                  , what = "Unexpected if-then-else expression."
                  , explain = SOME "Try using parentheses: (if ... then ... else ...)"
                  }

            else if isReserved Token.Raise at i then
              if anyOkay restriction then
                consume_expRaise infdict (i+1)
              else
                error
                  { pos = Token.getSource (tok i)
                  , what = "Unexpected raise exception."
                  , explain = SOME "Try using parentheses: (raise ...)"
                  }

            else if isReserved Token.Fn at i then
              if anyOkay restriction then
                consume_expFn infdict (i+1)
              else
                error
                  { pos = Token.getSource (tok i)
                  , what = "Unexpected beginning of anonymous function."
                  , explain = SOME "Try using parentheses: (fn ... => ...)"
                  }

            else if isReserved Token.While at i then
              if anyOkay restriction then
                consume_expWhile infdict (i+1)
              else
                error
                  { pos = Token.getSource (tok i)
                  , what = "Unexpected beginning of while-loop."
                  , explain = SOME "Try using parentheses: (while ... do ...)"
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
              anyOkay restriction
              andalso isReserved Token.Colon at i
            then
              (true, consume_expTyped exp (i+1))

            else if
              anyOkay restriction
              andalso isReserved Token.Handle at i
            then
              (true, consume_expHandle infdict exp (i+1))

            else if
              anyOkay restriction
              andalso (isReserved Token.Andalso at i
              orelse isReserved Token.Orelse at i)
            then
              (true, consume_expAndalsoOrOrelse infdict exp (i+1))

            else if
              infOkay restriction
              andalso Ast.Exp.isInfExp exp
              andalso check Token.isValueIdentifier at i
              andalso InfixDict.contains infdict (tok i)
            then
              (true, consume_expInfix infdict exp (i+1))

            else if
              appOkay restriction
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


      (** { label = exp [, ...] }
        *  ^
        *)
      and consume_expRecord infdict left i =
        let
          fun parseElem i =
            let
              val (i, lab) = parse_recordLabel i
              val (i, eq) = parse_reserved Token.Equal i
              val (i, exp) = consume_exp infdict NoRestriction i
            in
              ( i
              , { lab = lab
                , eq = eq
                , exp = exp
                }
              )
            end

            val (i, {elems, delims}) =
              parse_oneOrMoreDelimitedByReserved
                {parseElem = parseElem, delim = Token.Comma}
                i

            val (i, right) = parse_reserved Token.CloseCurlyBracket i
        in
          ( i
          , Ast.Exp.Record
              { left = left
              , elems = elems
              , delims = delims
              , right = right
              }
          )
        end


      (** # lab
        *  ^
        *)
      and consume_expSelect hash i =
        let
          val (i, lab) = parse_recordLabel i
        in
          ( i
          , Ast.Exp.Select
              { hash = hash
              , label = lab
              }
          )
        end


      and consume_expIfThenElse infdict iff i =
        let
          val (i, exp1) = consume_exp infdict NoRestriction i
          val (i, thenn) = parse_reserved Token.Then i
          val (i, exp2) = consume_exp infdict NoRestriction i
          val (i, elsee) = parse_reserved Token.Else i
          val (i, exp3) = consume_exp infdict NoRestriction i

          val result =
            Ast.Exp.IfThenElse
              { iff = iff
              , exp1 = exp1
              , thenn = thenn
              , exp2 = exp2
              , elsee = elsee
              , exp3 = exp3
              }

          (** NOTE: this is technically a noop, because `raise` has low enough
            * precedence that the left rotation will never happen. But I like
            * keeping the code here because it's informative.
            *)
          val result = FixExpPrecedence.maybeRotateLeft result
        in
          (i, result)
        end


      (** [ ... ]
        *  ^
        *)
      and consume_expListLiteral infdict i =
        let
          val openBracket = tok (i-1)

          fun finish elems delims closeBracket =
            Ast.Exp.List
              { left = openBracket
              , right = closeBracket
              , elems = elems
              , delims = delims
              }
        in
          if isReserved Token.CloseSquareBracket at i then
            (i+1, finish (Seq.empty ()) (Seq.empty ()) (tok i))
          else
            let
              val parseElem = consume_exp infdict NoRestriction
              val (i, {elems, delims}) =
                parse_oneOrMoreDelimitedByReserved
                  {parseElem = parseElem, delim = Token.Comma}
                  i
              val (i, closeBracket) =
                parse_reserved Token.CloseSquareBracket i
            in
              (i, finish elems delims closeBracket)
            end
        end


      (** case exp of match
        *     ^
        *)
      and consume_expCase infdict i =
        let
          val casee = tok (i-1)
          val (i, exp) = consume_exp infdict NoRestriction i
          val (i, off) = parse_reserved Token.Of i
          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = consume_matchElem infdict, delim = Token.Bar}
              i
        in
          ( i
          , Ast.Exp.Case
              { casee = casee
              , exp = exp
              , off = off
              , elems = elems
              , delims = delims
              }
          )
        end


      (**  pat => exp
        * ^
        *)
      and consume_matchElem infdict i =
        let
          val (i, pat) = consume_pat infdict NoRestriction i
          val (i, arrow) = parse_reserved Token.FatArrow i
          val (i, exp) = consume_exp infdict NoRestriction i
        in
          (i, {pat=pat, arrow=arrow, exp=exp})
        end


      (** fn pat => exp [| pat => exp ...]
        *   ^
        *)
      and consume_expFn infdict i =
        let
          val fnn = tok (i-1)
          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = consume_matchElem infdict, delim = Token.Bar}
              i
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
          val (i, vid) = parse_longvid i
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
          val (i, exp2) = consume_exp infdict InfRestriction i
        in
          ( i
          , makeInfixExp infdict (exp1, id, exp2)
          )
        end



      (** appexp atexp
        *       ^
        *)
      and consume_expApp infdict leftExp i =
        let
          (* val _ = print ("app\n") *)

          val (i, rightExp) = consume_exp infdict AtRestriction i
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
          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = consume_matchElem infdict, delim = Token.Bar}
              i

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


      (** while exp1 do exp2
        *      ^
        *)
      and consume_expWhile infdict i =
        let
          val whilee = tok (i-1)
          val (i, exp1) = consume_exp infdict NoRestriction i
          val (i, doo) = parse_reserved Token.Do i
          val (i, exp2) = consume_exp infdict NoRestriction i

          val result =
            Ast.Exp.While
              { whilee = whilee
              , exp1 = exp1
              , doo = doo
              , exp2 = exp2
              }

          val result = FixExpPrecedence.maybeRotateLeft result
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
            else if isReserved Token.OpenCurlyBracket at i then
              consume_tyRecord (tok i) (i+1)
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


      (** { label: ty [, ...] }
        *  ^
        *)
      and consume_tyRecord leftBracket i =
        let
          fun parseElem i =
            let
              val (i, lab) = parse_recordLabel i
              val (i, colon) = parse_reserved Token.Colon i
              val (i, ty) = consume_ty {permitArrows = true} i
            in
              ( i
              , { lab = lab
                , colon = colon
                , ty = ty
                }
              )
            end

          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseElem, delim = Token.Comma}
              i

          val (i, rightBracket) = parse_reserved Token.CloseCurlyBracket i
        in
          ( i
          , Ast.Ty.Record
              { left = leftBracket
              , elems = elems
              , delims = delims
              , right = rightBracket
              }
          )
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
          val (i, inn) = parse_reserved Token.In i

          val parseElem = consume_exp infdict NoRestriction
          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseElem, delim = Token.Semicolon}
              i

          val (i, endd) = parse_reserved Token.End i
        in
          ( i
          , Ast.Exp.LetInEnd
              { lett = lett
              , dec = dec
              , inn = inn
              , exps = elems
              , delims = delims
              , endd = endd
              }
          )
        end


      (** ( )
        *  ^
        * OR
        * ( exp [, exp ...] )
        *  ^
        * OR
        * ( exp [; exp ...] )
        *  ^
        *)
      and consume_expParensOrTupleOrUnitOrSequence infdict leftParen i =
        if isReserved Token.CloseParen at i then
          ( i+1
          , Ast.Exp.Unit
              { left = leftParen
              , right = tok i
              }
          )
        else
          let
            val parseElem = consume_exp infdict NoRestriction
            val (i, exp) = parseElem i
          in
            if isReserved Token.CloseParen at i then
              ( i+1
              , Ast.Exp.Parens
                  { left = leftParen
                  , exp = exp
                  , right = tok i
                  }
              )
            else
              let
                val delimType =
                  if isReserved Token.Comma at i then
                    Token.Comma
                  else if isReserved Token.Semicolon at i then
                    Token.Semicolon
                  else
                    error
                      { pos = Token.getSource leftParen
                      , what = "Unmatched paren."
                      , explain = NONE
                      }

                val (i, delim) = (i+1, tok i)

                val (i, {elems, delims}) =
                  parse_zeroOrMoreDelimitedByReserved
                    { parseElem = parseElem
                    , delim = delimType
                    , shouldStop = isReserved Token.CloseParen
                    }
                    i

                val (i, rightParen) = parse_reserved Token.CloseParen i

                val stuff =
                  { left = leftParen
                  , elems = Seq.append (Seq.singleton exp, elems)
                  , delims = Seq.append (Seq.singleton delim, delims)
                  , right = rightParen
                  }
              in
                case delimType of
                  Token.Comma =>
                    (i, Ast.Exp.Tuple stuff)
                | _ =>
                    (i, Ast.Exp.Sequence stuff)
              end
          end


      val infdict = InfixDict.initialTopLevel
      val (i, _, topdec) = consume_dec infdict 0

      val _ =
        if i >= numToks then ()
        else
          error
            { pos = Token.getSource (tok i)
            , what = "Unexpected token."
            , explain = SOME "Invalid start of top-level declaration!"
            }
    in
      Ast.Dec topdec
    end


end
