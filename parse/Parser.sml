(** Copyright (c) 2020-2021 Sam Westrick
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
  structure PT = ParseTy
  structure PP = ParsePat
  exception Error = ParserUtils.Error

  structure Restriction = ExpPatRestriction

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
        ParserUtils.nyi toks fname i


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
      fun parse_tyvars i =
        PS.tyvars toks i
      fun parse_sigid i =
        PS.sigid toks i
      fun parse_strid i =
        PS.strid toks i
      fun parse_vid i =
        PS.vid toks i
      fun parse_longvid i =
        PS.longvid toks i
      fun parse_tycon i =
        PS.tycon toks i
      fun parse_maybeLongTycon i =
        PS.maybeLongTycon toks i
      fun parse_ty i =
        PT.ty toks i


      fun parse_oneOrMoreDelimitedByReserved x i =
        PC.oneOrMoreDelimitedByReserved toks x i
      fun parse_two (p1, p2) state =
        PC.two (p1, p2) state
      fun parse_zeroOrMoreWhile c p s =
        PC.zeroOrMoreWhile c p s
      fun parse_oneOrMoreWhile c p s =
        PC.oneOrMoreWhile c p s


      (** ====================================================================
        * Modules
        *)


      fun parse_datdesc i =
        let
          fun parse_condesc i =
            let
              val (i, vid) = parse_vid i
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
              , { vid = vid
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
                  {parseElem = parse_condesc, delim = Token.Bar}
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

          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseElem, delim = Token.And}
              i
        in
          ( i
          , { elems = elems
            , delims = delims
            }
          )
        end

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
          , Ast.Sig.Val
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
          , Ast.Sig.Type
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
          , Ast.Sig.Eqtype
              { eqtypee = eqtypee
              , elems = elems
              , delims = delims
              }
          )
        end


      (** datatype datdesc
        *         ^
        * OR
        * datatype tycon = datatype longtycon
        *         ^
        *)
      fun consume_sigSpecDatatypeDeclarationOrReplication infdict i =
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
            val (i, {elems, delims}) = parse_datdesc i
          in
            ( i
            , Ast.Sig.Datatype
              { datatypee = datatypee
              , elems = elems
              , delims = delims
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
            ( i
            , Ast.Sig.ReplicateDatatype
              { left_datatypee = left_datatypee
              , left_id = left_id
              , eq = eq
              , right_datatypee = right_datatypee
              , right_id = right_id
              }
            )
          end


      (** exception vid [of ty] [and vid [of ty] ...]
        *          ^
        *)
      fun consume_sigSpecException infdict i =
        let
          val exceptionn = tok (i-1)
          fun parseOne i =
            let
              val (i, vid) = parse_vid i
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
              , { vid = vid
                , arg = arg
                }
              )
            end

          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseOne, delim = Token.And}
              i
        in
          ( i
          , Ast.Sig.Exception
              { exceptionn = exceptionn
              , elems = elems
              , delims= delims
              }
          )
        end


      (** structure strid : sigexp [and ...]
        *          ^
        *)
      fun consume_sigSpecStructure infdict i =
        let
          val structuree = tok (i-1)

          fun parseOne i =
            let
              val (i, id) = parse_vid i
              val (i, colon) = parse_reserved Token.Colon i
              val (i, sigexp) = consume_sigExp infdict i
            in
              ( i
              , { id = id
                , colon = colon
                , sigexp = sigexp
                }
              )
            end

          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseOne, delim = Token.And}
              i
        in
          ( i
          , Ast.Sig.Structure
              { structuree = structuree
              , elems = elems
              , delims = delims
              }
          )
        end


      (** include sigexp
        *        ^
        *)
      and consume_sigSpecInclude infdict i =
        let
          val includee = tok (i-1)
          val (i, sigexp) = consume_sigExp infdict i
        in
          ( i
          , Ast.Sig.Include
              { includee = includee
              , sigexp = sigexp
              }
          )
        end


      and consume_oneSigSpec infdict i =
        if isReserved Token.Val at i then
          consume_sigSpecVal infdict (i+1)
        else if isReserved Token.Type at i then
          consume_sigSpecType infdict (i+1)
        else if isReserved Token.Eqtype at i then
          consume_sigSpecEqtype infdict (i+1)
        else if isReserved Token.Datatype at i then
          consume_sigSpecDatatypeDeclarationOrReplication infdict (i+1)
        else if isReserved Token.Exception at i then
          consume_sigSpecException infdict (i+1)
        else if isReserved Token.Structure at i then
          consume_sigSpecStructure infdict (i+1)
        else if isReserved Token.Include at i then
          consume_sigSpecInclude infdict (i+1)
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
            Ast.Sig.Multiple
              { elems = Seq.map #1 specs
              , delims = Seq.map #2 specs
              }

          val result =
            case Seq.length specs of
              0 =>
                Ast.Sig.EmptySpec
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
      and consume_sigExpWhereType sigexp infdict i =
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
          , Ast.Sig.WhereType
              { sigexp = sigexp
              , elems = elems
              }
          )
        end


      (** sig spec end
        *    ^
        *)
      and consume_sigExpSigEnd infdict i =
        let
          val sigg = tok (i-1)
          val (i, spec) = consume_sigSpec infdict i
          val (i, endd) = parse_reserved Token.End i
        in
          ( i
          , Ast.Sig.Spec
              { sigg = sigg
              , spec = spec
              , endd = endd
              }
          )
        end


      and consume_sigExp infdict i =
        let
          val (i, sigexp) =
            if isReserved Token.Sig at i then
              consume_sigExpSigEnd infdict (i+1)
            else
              let
                val (i, sigid) = parse_sigid i
              in
                (i, Ast.Sig.Ident sigid)
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
            Ast.SigDec (Ast.Sig.Signature
              { signaturee = signaturee
              , elems = elems
              , delims = delims
              })
        in
          ((i, infdict), result)
        end

      (** ====================================================================
        * Structures and core declarations
        *
        * In the grammar, these are "strdecs" which include e.g.
        *   structure X = ...
        *   local <strdec> in <strdec> end
        * as well as core declarations including e.g.
        *   val x = ...
        *   infix ...
        *   type ...
        *)


      (** strexp : sigexp
        *         ^
        * also handles opaque constraints (strexp :> sigexp)
        *)
      fun consume_strexpConstraint infdict strexp colon i =
        let
          val (i, sigexp) = consume_sigExp infdict i
        in
          ( i
          , Ast.Str.Constraint
              { strexp = strexp
              , colon = colon
              , sigexp = sigexp
              }
          )
        end


      and consume_strexpStruct infdict structt i : (int * Ast.Str.strexp) =
        let
          val ((i, _), strdec) = consume_strdec (i, infdict)
          val (i, endd) = parse_reserved Token.End i
        in
          ( i
          , Ast.Str.Struct
              { structt = structt
              , strdec = strdec
              , endd = endd
              }
          )
        end


      (** funid ( strexp )
        *        ^
        * OR
        *
        * funid ( strdec )
        *        ^
        *)
      and consume_strexpFunApp infdict funid lparen i =
        if
          check Token.isDecStartToken i orelse
          check Token.isStrDecStartToken i
        then
          let
            val ((i, _), strdec) = consume_strdec (i, infdict)
            val (i, rparen) = parse_reserved Token.CloseParen i
          in
            ( i
            , Ast.Str.FunAppDec
                { funid = funid
                , lparen = lparen
                , strdec = strdec
                , rparen = rparen
                }
            )
          end
        else
          let
            val (i, strexp) = consume_strexp infdict i
            val (i, rparen) = parse_reserved Token.CloseParen i
          in
            ( i
            , Ast.Str.FunAppExp
                { funid = funid
                , lparen = lparen
                , strexp = strexp
                , rparen = rparen
                }
            )
          end


      and consume_strexp infdict i =
        let
          val (i, strexp) =
            if isReserved Token.Struct i then
              consume_strexpStruct infdict (tok i) (i+1)

            else if
              check Token.isStrIdentifier i andalso
              isReserved Token.OpenParen (i+1)
            then
              consume_strexpFunApp infdict (tok i) (tok (i+1)) (i+2)

            else if check Token.isMaybeLongStrIdentifier i then
              (i+1, Ast.Str.Ident (Ast.MaybeLong.make (tok i)))
            else
              nyi "consume_strexp" i
        in
          consume_afterStrexp infdict strexp i
        end


      and consume_afterStrexp infdict strexp i =
        let
          val (again, (i, strexp)) =
            if isReserved Token.Colon i orelse isReserved Token.ColonArrow i
            then
              (true, consume_strexpConstraint infdict strexp (tok i) (i+1))
            else
              (false, (i, strexp))
        in
          if again then
            consume_afterStrexp infdict strexp i
          else
            (i, strexp)
        end


      (** structure strid [constraint] = strexp [and strid = ...]
        *          ^
        * where the optional constraint is either
        *   : sigexp       (transparent constraint)
        *   :> sigexp      (opaque constraint)
        *)
      and consume_strdecStructure infdict structuree i =
        let
          fun parse_maybeConstraint infdict i =
            if isReserved Token.Colon i orelse isReserved Token.ColonArrow i then
              let
                val (i, colon) = (i+1, tok i)
                val (i, sigexp) = consume_sigExp infdict i
              in
                (i, SOME {colon = colon, sigexp = sigexp})
              end
            else
              (i, NONE)

          fun parseOne i =
            let
              val (i, strid) = parse_strid i
              val (i, constraint) = parse_maybeConstraint infdict i
              val (i, eq) = parse_reserved Token.Equal i
              val (i, strexp) = consume_strexp infdict i
            in
              ( i
              , { strid = strid
                , constraint = constraint
                , eq = eq
                , strexp = strexp
                }
              )
            end

          val (i, {elems, delims}) =
            parse_oneOrMoreDelimitedByReserved
              {parseElem = parseOne, delim = Token.And}
              i
        in
          ( i
          , Ast.Str.Structure
              { structuree = structuree
              , elems = elems
              , delims = delims
              }
          )
        end


      and consume_strdec (i, infdict) : ((int * InfixDict.t) * Ast.Str.strdec) =
        if isReserved Token.Structure i then
          let
            val (i, dec) = consume_strdecStructure infdict (tok i) (i+1)
          in
            ((i, infdict), dec)
          end
        else
          let
            val ((i, infdict), dec) =
              ParseExpAndDec.dec toks (i, infdict)
          in
            ( (i, infdict)
            , Ast.Str.CoreDec dec
            )
          end

      (** ====================================================================
        * Functors
        *)

      fun consume_fundec infdict i =
        nyi "consume_fundec" i

      (** ====================================================================
        * Top-level
        *)

      fun consume_topDecOne (i, infdict): ((int * InfixDict.t) * Ast.topdec) =
        if isReserved Token.Signature at i then
          consume_sigDec (i+1, infdict)
        else if isReserved Token.Functor at i then
          consume_fundec infdict (i+1)
        else if check Token.isStrDecStartToken at i orelse
                check Token.isDecStartToken at i then
          let
            val (xx, strdec) = consume_strdec (i, infdict)
          in
            (xx, Ast.StrDec strdec)
          end
        else
          ParserUtils.error
            { pos = Token.getSource (tok i)
            , what = "Unexpected token."
            , explain = SOME "Invalid start of top-level declaration."
            }

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
          ParserUtils.error
            { pos = Token.getSource (tok i)
            , what = "Unexpected token."
            , explain = SOME "Invalid start of top-level declaration!"
            }
    in
      Ast.Ast topdecs
    end


end
