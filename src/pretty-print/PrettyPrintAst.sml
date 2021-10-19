(** Copyright (c) 2020-2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettyPrintAst:
sig
  val pretty: Ast.t -> string
end =
struct

  structure Doc = TokenDoc
  open Doc
  open PrettyUtil

  infix 2 ++ $$ //
  fun x ++ y = beside (x, y)
  fun x $$ y = aboveOrSpace (x, y)
  fun x // y = aboveOrBeside (x, y)

  fun showTy ty = PrettyTy.show ty
  fun showPat pat = PrettyPat.show pat

  fun showTypbind (front, typbind: Ast.Exp.typbind as {elems, delims}) =
    let
      fun showOne (starter, {tyvars, tycon, ty, eq}) =
        group (
          separateWithSpaces
            [ SOME (token starter)
            , maybeShowSyntaxSeq tyvars token
            , SOME (token tycon)
            , SOME (token eq)
            ]
          $$
          (spaces 2 ++ showTy ty)
        )
    in
      Seq.iterate op$$
        (showOne (front, Seq.nth elems 0))
        (Seq.zipWith showOne (delims, Seq.drop elems 1))
    end


  fun showDatbind (front, datbind: Ast.Exp.datbind as {elems, delims}) =
    let
      fun showCon (starter, {opp, id, arg}) =
        starter
        ++ space ++
        group (
          separateWithSpaces
            [ Option.map token opp
            , SOME (token id)
            , Option.map (fn {off, ty} => token off $$ (spaces 2 ++ showTy ty)) arg
            ]
        )

      fun showOne (starter, {tyvars, tycon, eq, elems, delims}) =
        let
          val initial =
            group (
              separateWithSpaces
                [ SOME (token starter)
                , maybeShowSyntaxSeq tyvars token
                , SOME (token tycon)
                , SOME (token eq)
                ]
            )
        in
          group (
            initial
            $$
            ((*spaces 2 ++*)
              group (
                Seq.iterate op$$
                  (showCon (space, Seq.nth elems 0))
                  (Seq.zipWith showCon (Seq.map token delims, Seq.drop elems 1))
              ))
            )
        end
    in
      Seq.iterate op$$
        (showOne (front, Seq.nth elems 0))
        (Seq.zipWith showOne (delims, Seq.drop elems 1))
    end


  fun showDec dec =
    let
      open Ast.Exp
    in
      case dec of
        DecVal {vall, tyvars, elems, delims} =>
          let
            fun mk (delim, {recc, pat, eq, exp}) =
              group (
                separateWithSpaces
                  [ SOME (token delim)
                  , Option.map token recc
                  , SOME (showPat pat)
                  , SOME (token eq)
                  ]
                $$
                (spaces 2 ++ showExp exp)
              )

            val first =
              let
                val {recc, pat, eq, exp} = Seq.nth elems 0
              in
                group (
                  separateWithSpaces
                    [ SOME (token vall)
                    , maybeShowSyntaxSeq tyvars token
                    , Option.map token recc
                    , SOME (showPat pat)
                    , SOME (token eq)
                    ]
                  $$
                  (spaces 2 ++ showExp exp)
                )
              end
          in
            Seq.iterate op$$ first
              (Seq.map mk (Seq.zip (delims, Seq.drop elems 1)))
          end

      | DecFun {funn, tyvars, fvalbind={elems, delims}} =>
          let
            fun showArgs args =
              if Seq.length args = 0 then empty else
              Seq.iterate (fn (prev, p) => prev ++ space ++ showPat p)
                (showPat (Seq.nth args 0))
                (Seq.drop args 1)

            fun showInfixed larg id rarg =
              showPat larg
              ++ space
              ++ token id
              ++ space
              ++ showPat rarg

            fun showFNameArgs xx =
              case xx of
                PrefixedFun {opp, id, args} =>
                  separateWithSpaces
                    [ Option.map token opp
                    , SOME (token id)
                    , SOME (showArgs args)
                    ]
              | InfixedFun {larg, id, rarg} =>
                  showInfixed larg id rarg
              | CurriedInfixedFun {lparen, larg, id, rarg, rparen, args} =>
                  separateWithSpaces
                    [ SOME (token lparen ++ showInfixed larg id rarg ++ token rparen)
                    , SOME (showArgs args)
                    ]

            fun showColonTy {ty: Ast.Ty.t, colon: Token.t} =
              token colon ++ space ++ showTy ty

            fun showClause front {fname_args, ty, eq, exp} =
              separateWithSpaces front
              ++ space
              ++ group (
                separateWithSpaces
                  ( [ SOME (showFNameArgs fname_args)
                    , Option.map showColonTy ty
                    , SOME (token eq)
                    ] )
                $$
                (spaces 2 ++ showExp exp)
              )

            fun mkFunction (andDelim, {elems=innerElems, delims}) =
              let
                val delim =
                  case andDelim of
                    NONE       => funn
                  | SOME delim => delim
                val tyvars =
                  case andDelim of
                    NONE   => [maybeShowSyntaxSeq tyvars token]
                  | SOME _ => nil
              in
                Seq.iterate op$$
                  (showClause (SOME (token delim) :: tyvars) (Seq.nth innerElems 0))
                  (Seq.zipWith
                    (fn (delim, elem) => showClause [SOME (spaces 2 ++ token delim)] elem)
                    (delims, Seq.drop innerElems 1))
              end
          in
            Seq.iterate op$$
              (mkFunction (NONE, Seq.nth elems 0))
              (Seq.zipWith mkFunction (Seq.map SOME delims, Seq.drop elems 1))
          end

      | DecType {typee, typbind} =>
          showTypbind (typee, typbind)

      | DecDatatype {datatypee, datbind, withtypee} =>
          let
            val datbinds = showDatbind (datatypee, datbind)
          in
            case withtypee of
              SOME {withtypee, typbind} =>
                datbinds $$ showTypbind (withtypee, typbind)

            | _ => datbinds
          end

      | DecInfix {infixx, precedence, elems} =>
          separateWithSpaces
            [ SOME (token infixx)
            , Option.map token precedence
            , SOME (seqWithSpaces elems token)
            ]

      | DecInfixr {infixrr, precedence, elems} =>
          separateWithSpaces
            [ SOME (token infixrr)
            , Option.map token precedence
            , SOME (seqWithSpaces elems token)
            ]

      | DecNonfix {nonfixx, elems} =>
          token nonfixx ++ space ++ seqWithSpaces elems token

      | DecException {exceptionn, elems, delims} =>
          let
            fun showExbind exbind =
              case exbind of
                ExnNew {opp, id, arg} =>
                  separateWithSpaces
                    [ Option.map token opp
                    , SOME (token id)
                    , Option.map (fn {off, ty} =>
                        token off ++ space ++ showTy ty) arg
                    ]

              | ExnReplicate {opp, left_id, eq, right_id} =>
                  separateWithSpaces
                    [ Option.map token opp
                    , SOME (token left_id)
                    , SOME (token eq)
                    , SOME (token (MaybeLongToken.getToken right_id))
                    ]

            fun showOne (starter, elem) =
              token starter ++ space ++ showExbind elem
          in
            Seq.iterate op$$
              (showOne (exceptionn, Seq.nth elems 0))
              (Seq.zipWith showOne (delims, Seq.drop elems 1))
          end

      | DecLocal {locall, left_dec, inn, right_dec, endd} =>
          group (
            token locall
            $$
            (spaces 2 ++ showDec left_dec)
            $$
            token inn
            $$
            (spaces 2 ++ showDec right_dec)
            $$
            token endd
          )

      | DecMultiple {elems, delims} =>
          let
            fun f i =
              showDec (Seq.nth elems i)
              ++
              (case Seq.nth delims i of
                NONE => empty
              | SOME semicolon => token semicolon)
          in
            Util.loop (0, Seq.length elems) empty
              (fn (prev, i) => prev $$ f i)
          end

      | DecEmpty =>
          empty

      | DecOpen {openn, elems} =>
          token openn ++ space
          ++ seqWithSpaces elems (token o MaybeLongToken.getToken)

      | DecAbstype {abstypee, datbind, withtypee, withh, dec, endd} =>
          let
            val datbinds =
              showDatbind (abstypee, datbind)

            val bottom =
              group (
                token withh
                $$
                (spaces 2 ++ showDec dec)
                $$
                token endd
              )
          in
            group (
              case withtypee of
                SOME {withtypee, typbind} =>
                  datbinds
                  $$
                  showTypbind (withtypee, typbind)
                  $$
                  bottom

              | _ => datbinds $$ bottom
            )
          end

      | DecReplicateDatatype
        {left_datatypee, left_id, eq, right_datatypee, right_id} =>
          seqWithSpaces
            (Seq.fromList
              [ left_datatypee
              , left_id
              , eq
              , right_datatypee
              , MaybeLongToken.getToken right_id
              ])
            token
    end


  and showExp exp =
    let
      open Ast.Exp
    in
      case exp of
        Const tok =>
          token tok
      | Unit {left, right} =>
          token left ++ token right
      | Ident {opp, id} =>
          separateWithSpaces
            [ Option.map token opp
            , SOME (token (MaybeLongToken.getToken id))
            ]
      | Parens {left, exp, right} =>
          token left ++ showExp exp ++ token right
      | Tuple {left, elems, delims, right} =>
          sequence left delims right (Seq.map showExp elems)
      | Sequence {left, elems, delims, right} =>
          sequence left delims right (Seq.map showExp elems)
      | List {left, elems, delims, right} =>
          sequence left delims right (Seq.map showExp elems)
      | Record {left, elems, delims, right} =>
          let
            fun showRow {lab, eq, exp} =
              group (
                (token lab ++ space ++ token eq)
                $$
                (spaces 2 ++ showExp exp)
              )
          in
            sequence left delims right (Seq.map showRow elems)
          end
      | Select {hash, label} =>
          token hash ++ space ++ token label
      | App {left, right} =>
          group (showExp left $$ (spaces 2 ++ showExp right))
      | Infix {left, id, right} =>
          showExp left ++ space ++ token id ++ space ++ showExp right
      | Andalso {left, andalsoo, right} =>
          showExp left ++ space ++ token andalsoo ++ space ++ showExp right
      | Orelse {left, orelsee, right} =>
          showExp left ++ space ++ token orelsee ++ space ++ showExp right
      | Typed {exp, colon, ty} =>
          showExp exp ++ space ++ token colon ++ space ++ showTy ty
      | IfThenElse {iff, exp1, thenn, exp2, elsee, exp3} =>
          group (
            (token iff ++ space ++ showExp exp1 ++ space ++ token thenn)
            $$
            (spaces 2 ++ showExp exp2)
            $$
            token elsee
            $$
            (spaces 2 ++ showExp exp3)
          )
      | While {whilee, exp1, doo, exp2} =>
          group (
            group (
              token whilee
              $$
              (spaces 2 ++ showExp exp1)
              $$
              token doo
            )
            $$
            (spaces 2 ++ showExp exp2)
          )

      | Raise {raisee, exp} =>
          group (token raisee $$ (spaces 2 ++ showExp exp))

      | Handle {exp=expLeft, handlee, elems, delims} =>
          let
            val first = Seq.nth elems 0
            val rest = Seq.drop elems 1

            fun mk (delim, {pat, arrow, exp}) =
              group (
                (token delim ++ space ++ showPat pat ++ space ++ token arrow)
                $$
                (spaces 4 ++ showExp exp)
              )

            val {pat, arrow, exp} = first
            val initial =
              group (
                showExp expLeft
                $$
                group (
                  token handlee
                  $$
                  (spaces 2 ++ showPat pat ++ space ++ token arrow)
                  $$
                  (spaces 4 ++ showExp exp)
                )
              )
          in
            group (
              Seq.iterate
                (fn (prev, next) => prev $$ mk next)
                initial
                (Seq.zip (delims, rest))
            )
          end

      | Case {casee, exp=expTop, off, elems, delims} =>
          let
            val first = Seq.nth elems 0
            val rest = Seq.drop elems 1

            fun mk (delim, {pat, arrow, exp}) =
              group (
                (token delim ++ space ++ showPat pat ++ space ++ token arrow)
                $$
                (spaces 4 ++ showExp exp)
              )

            val {pat, arrow, exp} = first
            val initial =
              (token casee ++ space ++
              showExp expTop ++ space ++ token off)
              $$
              (spaces 2 ++
                group (
                  (showPat pat ++ space ++ token arrow)
                  $$
                  (spaces 2 ++ showExp exp)
                )
              )
          in
            group (
              Seq.iterate
                (fn (prev, next) => prev $$ mk next)
                initial
                (Seq.zip (delims, rest))
            )
          end

      | Fn {fnn, elems, delims} =>
          let
            val first = Seq.nth elems 0
            val rest = Seq.drop elems 1

            fun mk (delim, {pat, arrow, exp}) =
              space ++
              group (
                (token delim ++ space ++ showPat pat ++ space ++ token arrow)
                $$
                (spaces 4 ++ showExp exp)
              )

            val {pat, arrow, exp} = first
            val initial =
              group (
                (token fnn ++ space ++ showPat pat ++ space ++ token arrow)
                $$
                (spaces 4 ++ showExp exp)
              )
          in
            group (
              Seq.iterate
                (fn (prev, next) => prev $$ mk next)
                initial
                (Seq.zip (delims, rest))
            )
          end

      | LetInEnd {lett, dec, inn, exps, delims, endd} =>
          let
            val prettyDec = showDec dec
            val numExps = Seq.length exps

            fun d i = Seq.nth delims i

            val withDelims = Seq.mapIdx (fn (i, e) =>
                showExp e ++ (if i = numExps - 1 then empty else token (d i)))
              exps

            val topPart =
              token lett
              $$
              (spaces 2 ++ prettyDec)
              $$
              token inn

            val topPart =
              if Ast.Exp.isMultipleDecs dec then
                topPart
              else
                group topPart
          in
            group (
              topPart
              $$
              (spaces 2 ++ group (Seq.iterate op$$ empty withDelims))
              $$
              token endd
            )
          end

      | MLtonSpecific {underscore, directive, contents, semicolon} =>
          token underscore ++ token directive
          ++ space ++ seqWithSpaces contents token ++ token semicolon
    end



  fun showSpec spec =
    case spec of
      Ast.Sig.EmptySpec =>
        empty

    | Ast.Sig.Val {vall, elems, delims} =>
        let
          fun showOne (starter, {vid, colon, ty}) =
            token starter
            ++ space ++
            token vid ++ space ++ token colon ++ space
            ++ showTy ty
        in
          Seq.iterate op$$
            (showOne (vall, Seq.nth elems 0))
            (Seq.zipWith showOne (delims, Seq.drop elems 1))
        end

    | Ast.Sig.Type {typee, elems, delims} =>
        let
          fun showOne (starter, {tyvars, tycon}) =
            separateWithSpaces
              [ SOME (token starter)
              , maybeShowSyntaxSeq tyvars token
              , SOME (token tycon)
              ]
        in
          Seq.iterate op$$
            (showOne (typee, Seq.nth elems 0))
            (Seq.zipWith showOne (delims, Seq.drop elems 1))
        end

    | Ast.Sig.TypeAbbreviation {typee, elems, delims} =>
        let
          fun showOne (starter, {tyvars, tycon, eq, ty}) =
            separateWithSpaces
              [ SOME (token starter)
              , maybeShowSyntaxSeq tyvars token
              , SOME (token tycon)
              , SOME (token eq)
              , SOME (showTy ty)
              ]
        in
          Seq.iterate op$$
            (showOne (typee, Seq.nth elems 0))
            (Seq.zipWith showOne (delims, Seq.drop elems 1))
        end

    | Ast.Sig.Eqtype {eqtypee, elems, delims} =>
        let
          fun showOne (starter, {tyvars, tycon}) =
            separateWithSpaces
              [ SOME (token starter)
              , maybeShowSyntaxSeq tyvars token
              , SOME (token tycon)
              ]
        in
          Seq.iterate op$$
            (showOne (eqtypee, Seq.nth elems 0))
            (Seq.zipWith showOne (delims, Seq.drop elems 1))
        end

    | Ast.Sig.Datatype {datatypee, elems, delims} =>
        (** This is *really* similar to a datbind (see showDatbind above), but
          * only one difference: there is no possible 'op' in the condesc, ugh.
          *)
        let
          fun showCon (starter, {vid, arg}) =
            starter
            ++ space ++
            group (
              separateWithSpaces
                [ SOME (token vid)
                , Option.map (fn {off, ty} =>
                    token off $$ (spaces 2 ++ showTy ty)) arg
                ]
            )

          fun show_datdesc (starter, {tyvars, tycon, eq, elems, delims}) =
            let
              val initial =
                group (
                  separateWithSpaces
                    [ SOME (token starter)
                    , maybeShowSyntaxSeq tyvars token
                    , SOME (token tycon)
                    , SOME (token eq)
                    ]
                )
            in
              group (
                initial
                $$
                ((*spaces 2 ++*)
                  group (
                    Seq.iterate op$$
                      (showCon (space, Seq.nth elems 0))
                      (Seq.zipWith showCon (Seq.map token delims, Seq.drop elems 1))
                  ))
              )
            end
        in
          Seq.iterate op$$
            (show_datdesc (datatypee, Seq.nth elems 0))
            (Seq.zipWith show_datdesc (delims, Seq.drop elems 1))
        end

    | Ast.Sig.ReplicateDatatype
      {left_datatypee, left_id, eq, right_datatypee, right_id} =>
        group (
          separateWithSpaces
            [ SOME (token left_datatypee)
            , SOME (token left_id)
            , SOME (token eq)
            ]
          $$
          (spaces 2 ++
            token right_datatypee
            ++ space ++
            token (MaybeLongToken.getToken right_id))
        )

    | Ast.Sig.Exception {exceptionn, elems, delims} =>
        let
          fun showOne (starter, {vid, arg}) =
              group (
                separateWithSpaces
                  [ SOME (token starter)
                  , SOME (token vid)
                  , Option.map (fn {off, ty} => token off ++ space ++ showTy ty) arg
                  ]
              )
        in
          Seq.iterate op$$
            (showOne (exceptionn, Seq.nth elems 0))
            (Seq.zipWith showOne (delims, Seq.drop elems 1))
        end

    | Ast.Sig.Structure {structuree, elems, delims} =>
        let
          fun showOne (starter, {id, colon, sigexp}) =
            group (
              separateWithSpaces
                [ SOME (token starter)
                , SOME (token id)
                , SOME (token colon)
                ]
              $$
              (spaces 2 ++ showSigExp sigexp)
            )
        in
          Seq.iterate op$$
            (showOne (structuree, Seq.nth elems 0))
            (Seq.zipWith showOne (delims, Seq.drop elems 1))
        end

    | Ast.Sig.Include {includee, sigexp} =>
        group (
          token includee
          $$
          (spaces 2 ++ showSigExp sigexp)
        )

    | Ast.Sig.IncludeIds {includee, sigids} =>
        Seq.iterate (fn (a, b) => a ++ space ++ token b)
          (token includee)
          sigids

    | Ast.Sig.Multiple {elems, delims} =>
        let
          fun showOne (elem: Ast.Sig.spec, delim: Token.t option) =
            showSpec elem
            ++
            (case delim of NONE => empty | SOME sc => token sc)
        in
          Seq.iterate op$$ empty (Seq.zipWith showOne (elems, delims))
        end

    | Ast.Sig.Sharing {spec, sharingg, elems, delims} =>
        let
          fun showOne (delim, elem) =
            token delim (** this is an '=' *)
            ++ space
            ++ token (MaybeLongToken.getToken elem)

          val stuff =
            Seq.iterate
              (fn (a, b) => a ++ space ++ b)
              (token (MaybeLongToken.getToken (Seq.nth elems 0)))
              (Seq.zipWith showOne (delims, Seq.drop elems 1))
        in
          group (
            showSpec spec
            $$
            (token sharingg ++ space ++ stuff)
          )
        end

    | Ast.Sig.SharingType {spec, sharingg, typee, elems, delims} =>
        let
          fun showOne (delim, elem) =
            token delim (** this is an '=' *)
            ++ space
            ++ token (MaybeLongToken.getToken elem)

          val stuff =
            Seq.iterate
              (fn (a, b) => a ++ space ++ b)
              (token (MaybeLongToken.getToken (Seq.nth elems 0)))
              (Seq.zipWith showOne (delims, Seq.drop elems 1))
        in
          group (
            showSpec spec
            $$
            (token sharingg ++ space ++ token typee ++ space ++ stuff)
          )
        end


  and showSigExp sigexp =
    case sigexp of
      Ast.Sig.Ident id =>
        token id

    | Ast.Sig.Spec {sigg, spec, endd} =>
        group (
          token sigg
          $$
          (spaces 2 ++ showSpec spec)
          $$
          token endd
        )

    | Ast.Sig.WhereType {sigexp, elems} =>
        let
          val se = showSigExp sigexp

          fun showElem {wheree, typee, tyvars, tycon, eq, ty} =
            separateWithSpaces
              [ SOME (token wheree) (** this could be 'and' *)
              , SOME (token typee)
              , maybeShowSyntaxSeq tyvars token
              , SOME (token (MaybeLongToken.getToken tycon))
              , SOME (token eq)
              , SOME (showTy ty)
              ]
        in
          Seq.iterate op$$ se (Seq.map showElem elems)
        end


  fun showSigDec (Ast.Sig.Signature {signaturee, elems, delims}) =
    let
      fun showOne (starter, {ident, eq, sigexp}) =
        group (
          (token starter
          ++ space ++ token ident ++ space ++ token eq)
          $$
          (spaces 2 ++ showSigExp sigexp)
        )
    in
      Seq.iterate op$$
        (showOne (signaturee, Seq.nth elems 0))
        (Seq.zipWith showOne (delims, Seq.drop elems 1))
    end


  fun showStrExp e =
    case e of
      Ast.Str.Ident id =>
        token (MaybeLongToken.getToken id)

    | Ast.Str.Struct {structt, strdec, endd} =>
        group (
          token structt
          $$
          (spaces 2 ++ showStrDec strdec)
          $$
          token endd
        )

    | Ast.Str.Constraint {strexp, colon, sigexp} =>
        showStrExp strexp
        ++ space ++ token colon
        ++ space ++ showSigExp sigexp

    | Ast.Str.FunAppExp {funid, lparen, strexp, rparen} =>
        token funid ++ space
        ++ token lparen ++ showStrExp strexp ++ token rparen

    | Ast.Str.FunAppDec {funid, lparen, strdec, rparen} =>
        token funid ++ space
        ++ token lparen ++ showStrDec strdec ++ token rparen

    | Ast.Str.LetInEnd {lett, strdec, inn, strexp, endd} =>
        let
          val prettyDec = showStrDec strdec
          val prettyExp = showStrExp strexp

          val topPart =
            token lett
            $$
            (spaces 2 ++ prettyDec)
            $$
            token inn

          val topPart =
            if Ast.Str.isMultipleDecs strdec then
              topPart
            else
              group topPart
        in
          group (
            topPart
            $$
            (spaces 2 ++ group (prettyExp))
            $$
            token endd
          )
        end


  and showStrDec d =
    case d of
      Ast.Str.DecEmpty =>
        empty

    | Ast.Str.DecCore d =>
        showDec d

    | Ast.Str.DecStructure {structuree, elems, delims} =>
        let
          fun maybeShowConstraint constraint =
            case constraint of
              NONE => NONE
            | SOME {colon, sigexp} =>
                SOME (token colon ++ space ++ showSigExp sigexp)

          fun showOne (starter, {strid, constraint, eq, strexp}) =
            group (
              separateWithSpaces
                [ SOME (token starter)
                , SOME (token strid)
                , maybeShowConstraint constraint
                , SOME (token eq)
                ]
              $$
              (spaces 2 ++ showStrExp strexp)
            )
        in
          Seq.iterate op$$
            (showOne (structuree, Seq.nth elems 0))
            (Seq.map showOne (Seq.zip (delims, (Seq.drop elems 1))))
        end

    | Ast.Str.DecMultiple {elems, delims} =>
        let
          fun f i =
            showStrDec (Seq.nth elems i)
            ++
            (case Seq.nth delims i of
              NONE => empty
            | SOME sc => token sc)
        in
          Util.loop (0, Seq.length elems) empty (fn (prev, i) => prev $$ f i)
        end

    | Ast.Str.DecLocalInEnd {locall, strdec1, inn, strdec2, endd} =>
        let
          val topPart =
            token locall
            $$
            (spaces 2 ++ showStrDec strdec1)
            $$
            token inn

          val topPart =
            if Ast.Str.isMultipleDecs strdec1 then
              topPart
            else
              group topPart
        in
          group (
            topPart
            $$
            (spaces 2 ++ group (showStrDec strdec2))
            $$
            token endd
          )
        end

    (** This is MLton-specific. Useful for testing by parsing the entire
      * MLton implementation of the standard basis.
      *)
    | Ast.Str.MLtonOverload
      {underscore, overload, prec, name, colon, ty, ass, elems, delims} =>
        let
          val front =
            token underscore ++ token overload
            ++ space ++ token prec
            ++ space ++ token name
            ++ space ++ token colon
            ++ space ++ showTy ty
            ++ space ++ token ass

          fun showOne (d, e) =
            token d ++ space ++ token (MaybeLongToken.getToken e)
        in
          group (
            front
            $$
            (spaces 2 ++
              Seq.iterate op$$
                (token (MaybeLongToken.getToken (Seq.nth elems 0)))
                (Seq.zipWith showOne (delims, Seq.drop elems 1)))
          )
        end


  fun showFunArg fa =
    case fa of
      Ast.Fun.ArgSpec spec => showSpec spec
    | Ast.Fun.ArgIdent {strid, colon, sigexp} =>
        group (
          token strid
          ++ space ++ token colon ++ space ++
          showSigExp sigexp
        )

  fun showFunDec (Ast.Fun.DecFunctor {functorr, elems, delims}) =
    let
      fun showFunctor
        (starter, {funid, lparen, funarg, rparen, constraint, eq, strexp})
        =
        group (
          group (
            group (
              token starter
              ++ space ++
              token funid
              ++ space ++ token lparen ++ showFunArg funarg ++ token rparen
            )
            $$
            (case constraint of NONE => empty | SOME {colon, sigexp} =>
              spaces 2 ++ token colon ++ space ++ showSigExp sigexp)
            ++
            space ++ token eq
          )
          $$
          showStrExp strexp
        )
    in
      Seq.iterate op$$
        (showFunctor (functorr, Seq.nth elems 0))
        (Seq.map showFunctor (Seq.zip (delims, Seq.drop elems 1)))
    end


  fun showAst (Ast.Ast tds) =
    if Seq.length tds = 0 then
      empty
    else
      let
        fun showOne {topdec, semicolon} =
          let
            val td =
              case topdec of
                Ast.StrDec d => showStrDec d
              | Ast.SigDec d => showSigDec d
              | Ast.FunDec d => showFunDec d
            val sc =
              case semicolon of
                NONE => empty
              | SOME sc => token sc
          in
            td ++ sc
          end

        val all = Seq.map showOne tds
      in
        Seq.iterate op$$ (Seq.nth all 0) (Seq.drop all 1)
      end


  fun pretty ast =
    let
      val doc = showAst ast
      val doc = TokenDoc.insertComments doc
    in
      StringDoc.toString (toStringDoc doc)
    end

end
