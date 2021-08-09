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

  structure PC = ParserCombinators
  structure PS = ParseSimple
  exception Error = PS.Error

  fun error (info as {what, pos, explain}) =
    PS.error info

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


      fun parse_reserved rc i =
        PS.reserved toks rc i
      fun parse_tyvar i =
        PS.tyvar toks i
      fun parse_sigid i =
        PS.sigid toks i
      fun parse_maybeReserved rc i =
        PS.maybeReserved toks rc i
      fun parse_vid i =
        PS.vid toks i
      fun parse_longvid i =
        PS.longvid toks i
      fun parse_recordLabel i =
        PS.recordLabel toks i
      fun parse_tycon i =
        PS.tycon toks i
      fun parse_maybeLongTycon i =
        PS.maybeLongTycon toks i
      fun parse_ty i =
        ParseTy.ty toks i


      fun parse_zeroOrMoreDelimitedByReserved x i =
        PC.zeroOrMoreDelimitedByReserved toks x i
      fun parse_oneOrMoreDelimitedByReserved x i =
        PC.oneOrMoreDelimitedByReserved toks x i
      fun parse_two (p1, p2) state =
        PC.two (p1, p2) state
      fun parse_zeroOrMoreWhile c p s =
        PC.zeroOrMoreWhile c p s
      fun parse_oneOrMoreWhile c p s =
        PC.oneOrMoreWhile c p s


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


      fun check_normalOrOpInfix infdict opp vid =
        if InfixDict.contains infdict vid andalso not (Option.isSome opp) then
          error
            { pos = Token.getSource vid
            , what = "Infix identifier not prefaced by 'op'"
            , explain = NONE
            }
        else
          ()


      fun consume_opvid infdict i =
        let
          val (i, opp) = parse_maybeReserved Token.Op i
          val (i, vid) = parse_vid i
          val _ = check_normalOrOpInfix infdict opp vid
        in
          (i, {opp = opp, vid = vid})
        end


      fun parse_typbind i =
        let
          fun parseElem i =
            let
              val (i, tyvars) = parse_tyvars i
              val (i, tycon) = parse_tycon i
              val (i, eq) = parse_reserved Token.Equal i
              val (i, ty) = parse_ty i
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
          (i, typbind)
        end


      and parse_datbind i =
        let
          fun parseConbind i =
            let
              val (i, opp) = parse_maybeReserved Token.Op i
              val (i, id) = parse_vid i
              val (i, arg) =
                if not (isReserved Token.Of at i) then
                  (i, NONE)
                else
                  let
                    val off = tok i
                    val (i, ty) = parse_ty (i+1)
                  in
                    (i, SOME {off = off, ty = ty})
                  end
            in
              ( i
              , { opp = opp
                , id = id
                , arg = arg
                }
              )
            end

          fun parseElem i =
            let
              val (i, tyvars) = parse_tyvars i
              val (i, tycon) = parse_vid i
              val (i, eq) = parse_reserved Token.Equal i

              val (i, {elems, delims}) =
                parse_oneOrMoreDelimitedByReserved
                  {parseElem = parseConbind, delim = Token.Bar}
                  i
            in
              ( i
              , { tyvars = tyvars
                , tycon = tycon
                , eq = eq
                , elems = elems
                , delims = delims
                }
              )
            end

          val (i, datbind) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseElem, delim = Token.And}
              i
        in
          (i, datbind)
        end


      (** ================================================================= *)

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
                  val (i, ty) = parse_ty i
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
          val (i, ty) = parse_ty i
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


      (** ================================================================= *)

      fun consume_dec infdict i =
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
            parse_zeroOrMoreWhile
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
        else if isReserved Token.Datatype at i then
          consume_decDatatypeDeclarationOrReplication (i+1, infdict)
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
          val (i, opp) = parse_maybeReserved Token.Op i
          val (i, vid) = parse_vid i
        in
          if isReserved Token.Of at i then
            let
              val (i, off) = (i+1, tok i)
              val (i, ty) = parse_ty i
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
              val (i, opp) = parse_maybeReserved Token.Op i
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
                    parse_zeroOrMoreWhile
                      (fn i => not (isReserved Token.Colon at i orelse isReserved Token.Equal at i))
                      (consume_pat infdict AtRestriction)
                      i

                  val (i, ty) =
                    if not (isReserved Token.Colon at i) then
                      (i, NONE)
                    else
                      let
                        val colon = tok i
                        val (i, ty) = parse_ty (i+1)
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
              (i, Seq.fromRevList acc)

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
              (i, Seq.fromRevList acc)

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
          val (i, typbind) = parse_typbind i
        in
          ( (i, infdict)
          , Ast.Exp.DecType
              { typee = typee
              , typbind = typbind
              }
          )
        end


      (** datatype datbind <withtype typbind>
        *         ^
        * OR
        * datatype tycon = datatype longtycon
        *         ^
        *)
      and consume_decDatatypeDeclarationOrReplication (i, infdict) =
        if (
          isReserved Token.OpenParen at i (* datatype ('a, ...) foo *)
          orelse check Token.isTyVar at i (* datatype 'a foo *)
          orelse ( (* datatype foo = A *)
            check Token.isValueIdentifierNoEqual at i
            andalso isReserved Token.Equal at (i+1)
            andalso not (isReserved Token.Datatype at (i+2))
          )
        ) then
          let
            val datatypee = tok (i-1)
            val (i, datbind) = parse_datbind i
            val (i, withtypee) =
              if not (isReserved Token.Withtype at i) then
                (i, NONE)
              else
                let
                  val withtypee = tok i
                  val (i, typbind) = parse_typbind (i+1)
                in
                  (i, SOME {withtypee = withtypee, typbind = typbind})
                end
          in
            ( (i, infdict)
            , Ast.Exp.DecDatatype
              { datatypee = datatypee
              , datbind = datbind
              , withtypee = withtypee
              }
            )
          end
        else
          let
            val left_datatypee = tok (i-1)
            val (i, left_id) = parse_vid i
            val (i, eq) = parse_reserved Token.Equal i
            val (i, right_datatypee) = parse_reserved Token.Datatype i
            val (i, right_id) = parse_longvid i
          in
            ( (i, infdict)
            , Ast.Exp.DecReplicateDatatype
              { left_datatypee = left_datatypee
              , left_id = left_id
              , eq = eq
              , right_datatypee = right_datatypee
              , right_id = right_id
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
              val (i, recc) = parse_maybeReserved Token.Rec i
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
          val (i, ty) = parse_ty i
        in
          ( i
          , Ast.Exp.Typed
              { exp = exp
              , colon = colon
              , ty = ty
              }
          )
        end


      (** ================================================================= *)


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

      (** ====================================================================
        * Modules
        *)


      (** val vid : ty [and ...]
        *    ^
        *)
      fun consume_sigSpecVal infdict i =
        let
          val vall = tok (i-1)

          fun parseOne i =
            let
              val (i, vid) = parse_vid i
              val (i, colon) = parse_reserved Token.Colon i
              val (i, ty) = parse_ty i
            in
              ( i
              , { vid = vid
                , colon = colon
                , ty = ty
                }
              )
            end

          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseOne, delim = Token.And}
              i
        in
          ( i
          , Ast.Module.Val
              { vall = vall
              , elems = elems
              , delims = delims
              }
          )
        end


      (** type tyvarseq tycon [and ...]
        *     ^
        *)
      fun consume_sigSpecType infdict i =
        let
          val typee = tok (i-1)

          fun parseOne i =
            let
              val (i, tyvars) = parse_tyvars i
              val (i, tycon) = parse_tycon i
            in
              ( i
              , { tyvars = tyvars
                , tycon = tycon
                }
              )
            end

          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseOne, delim = Token.And}
              i
        in
          ( i
          , Ast.Module.Type
              { typee = typee
              , elems = elems
              , delims = delims
              }
          )
        end


      (** eqtype tyvars tycon [and ...]
        *       ^
        *)
      fun consume_sigSpecEqtype infdict i =
        let
          val eqtypee = tok (i-1)

          fun parseOne i =
            let
              val (i, tyvars) = parse_tyvars i
              val (i, tycon) = parse_tycon i
            in
              ( i
              , { tyvars =  tyvars
                , tycon = tycon
                }
              )
            end

          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseOne, delim = Token.And}
              i
        in
          ( i
          , Ast.Module.Eqtype
              { eqtypee = eqtypee
              , elems = elems
              , delims = delims
              }
          )
        end

      fun consume_oneSigSpec infdict i =
        if isReserved Token.Val at i then
          consume_sigSpecVal infdict (i+1)
        else if isReserved Token.Type at i then
          consume_sigSpecType infdict (i+1)
        else if isReserved Token.Eqtype at i then
          consume_sigSpecEqtype infdict (i+1)
        else
          nyi "consume_oneSigSpec" i


      and consume_sigSpec infdict i =
        let
          fun consume_maybeSemicolon i =
            if isReserved Token.Semicolon at i then
              (i+1, SOME (tok i))
            else
              (i, NONE)

          val (i, specs) =
            parse_zeroOrMoreWhile
              (fn i => check Token.isSigSpecStartToken at i)
              ( parse_two
                ( consume_oneSigSpec infdict
                , consume_maybeSemicolon
                )
              )
              i

          fun makeSpecMultiple () =
            Ast.Module.Multiple
              { elems = Seq.map #1 specs
              , delims = Seq.map #2 specs
              }

          val result =
            case Seq.length specs of
              0 =>
                Ast.Module.EmptySpec
            | 1 =>
                let
                  val (spec, semicolon) = Seq.nth specs 0
                in
                  if isSome semicolon then
                    makeSpecMultiple ()
                  else
                    spec
                end
            | _ =>
                makeSpecMultiple ()
        in
          (i, result)
        end


      (** sigexp where type tyvarseq tycon = ty [and/where type ...]
        *       ^
        *)
      fun consume_sigExpWhereType sigexp infdict i =
        let
          fun nextIsWhereOrAndType i =
            (isReserved Token.Where at i orelse isReserved Token.And at i)
            andalso
            (isReserved Token.Type at i+1)

          fun parseOne i =
            let
              val (i, wheree) = (i+1, tok i)
              val (i, typee) = parse_reserved Token.Type i
              val (i, tyvars) = parse_tyvars i
              val (i, tycon) = parse_maybeLongTycon i
              val (i, eq) = parse_reserved Token.Equal i
              val (i, ty) = parse_ty i
            in
              ( i
              , { wheree = wheree
                , typee = typee
                , tyvars = tyvars
                , tycon = tycon
                , eq = eq
                , ty = ty
                }
              )
            end

          val (i, elems) =
            parse_oneOrMoreWhile nextIsWhereOrAndType parseOne i
        in
          ( i
          , Ast.Module.WhereType
              { sigexp = sigexp
              , elems = elems
              }
          )
        end


      (** sig spec end
        *    ^
        *)
      fun consume_sigExpSigEnd infdict i =
        let
          val sigg = tok (i-1)
          val (i, spec) = consume_sigSpec infdict i
          val (i, endd) = parse_reserved Token.End i
        in
          ( i
          , Ast.Module.Spec
              { sigg = sigg
              , spec = spec
              , endd = endd
              }
          )
        end


      fun consume_sigExp infdict i =
        let
          val (i, sigexp) =
            if isReserved Token.Sig at i then
              consume_sigExpSigEnd infdict (i+1)
            else
              let
                val (i, sigid) = parse_sigid i
              in
                (i, Ast.Module.Ident sigid)
              end
        in
          if isReserved Token.Where at i then
            consume_sigExpWhereType sigexp infdict i
          else
            (i, sigexp)
        end

      (** signature sigid = sigexp [and ...]
        *          ^
        *)
      fun consume_sigDec (i, infdict) : ((int * InfixDict.t) * Ast.topdec) =
        let
          val signaturee = tok (i-1)

          fun parseOne i =
            let
              val (i, sigid) = parse_sigid i
              val (i, eq) = parse_reserved Token.Equal i
              val (i, sigexp) = consume_sigExp infdict i
            in
              ( i
              , { ident = sigid
                , eq = eq
                , sigexp = sigexp
                }
              )
            end

          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseOne, delim = Token.And}
              i

          val result: Ast.topdec =
            Ast.SigDec (Ast.Module.Signature
              { signaturee = signaturee
              , elems = elems
              , delims = delims
              })
        in
          ((i, infdict), result)
        end

      (** ====================================================================
        * Top-level
        *)

      fun consume_topDecOne (i, infdict): ((int * InfixDict.t) * Ast.topdec) =
        if isReserved Token.Signature at i then
          consume_sigDec (i+1, infdict)
        else
          let
            val (i, infdict, dec) = consume_dec infdict i
          in
            ( (i, infdict)
            , Ast.StrDec (Ast.Module.Dec dec)
            )
          end

      val infdict = InfixDict.initialTopLevel

      val ((i, _), topdecs) =
        parse_zeroOrMoreWhile
          (fn (i, _) => i < numToks)
          consume_topDecOne
          (0, infdict)

      (* val (i, _, topdec) = consume_dec infdict 0 *)

      val _ =
        if i >= numToks then ()
        else
          error
            { pos = Token.getSource (tok i)
            , what = "Unexpected token."
            , explain = SOME "Invalid start of top-level declaration!"
            }
    in
      Ast.Ast topdecs
    end


end
