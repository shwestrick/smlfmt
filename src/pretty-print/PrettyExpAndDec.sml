(** Copyright (c) 2020-2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettyExpAndDec:
sig
  val showExp: Ast.Exp.exp -> TokenDoc.t
  val showDec: Ast.Exp.dec -> TokenDoc.t
end =
struct

  open TokenDoc
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

end
