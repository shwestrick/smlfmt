(** Copyright (c) 2020-2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettyPrintAst:
sig
  val pretty: Ast.t -> string
end =
struct

  structure PD = StringDoc
  open PD

  infix 2 ++ $$ //
  fun x ++ y = beside (x, y)
  fun x $$ y = aboveOrSpace (x, y)
  fun x // y = aboveOrBeside (x, y)

  fun token x = text (Token.toString x)

  fun seqWithSpaces elems f =
    if Seq.length elems = 0 then empty else
    Seq.iterate
      (fn (prev, tok) => prev ++ space ++ f tok)
      (f (Seq.nth elems 0))
      (Seq.drop elems 1)

  fun spaces n =
    List.foldl op++ empty (List.tabulate (n, fn _ => space))

  fun parensAround (x: doc) =
    text "(" ++ x ++ text ")"

  fun sequence openn delims close (xs: PD.t Seq.t) =
    if Seq.length xs = 0 then
      token openn ++ token close
    else
      let
        val top = token openn ++ softspace ++ Seq.nth xs 0
        fun f (delim, x) = token delim ++ space ++ x
      in
        group (
          Seq.iterate op// top (Seq.map f (Seq.zip (delims, Seq.drop xs 1)))
          //
          token close
        )
      end


  fun separateWithSpaces (items: doc option list) : doc =
    let
      val items: doc list = List.mapPartial (fn x => x) items
    in
      case items of
        [] => empty
      | first :: rest =>
          List.foldl (fn (next, prev) => prev ++ space ++ next) first rest
    end


  fun maybeShowSyntaxSeq s f =
    case s of
      Ast.SyntaxSeq.Empty => NONE
    | Ast.SyntaxSeq.One x => SOME (f x)
    | Ast.SyntaxSeq.Many {left, elems, delims, right} =>
        SOME (sequence left delims right (Seq.map f elems))


  fun showTy ty =
    let
      open Ast.Ty
    in
      case ty of
        Var tok =>
          token tok
      | Con {args = Ast.SyntaxSeq.Empty, id} =>
          token (MaybeLongToken.getToken id)
      | Con {args, id} =>
          (separateWithSpaces
            [ maybeShowSyntaxSeq args showTy
            , SOME (token (MaybeLongToken.getToken id))
            ])
      | Parens {left, ty, right} =>
          token left ++ showTy ty ++ token right
      | Tuple {elems, delims} =>
          let
            val begin = showTy (Seq.nth elems 0)
            fun f (delim, x) = space ++ token delim ++ space ++ showTy x
          in
            Seq.iterate op++ begin
              (Seq.map f (Seq.zip (delims, Seq.drop elems 1)))
          end
      | Record {left, elems, delims, right} =>
          let
            fun showElem {lab, colon, ty} =
              token lab ++ space ++ token colon
              ++ space ++ showTy ty
          in
            sequence left delims right (Seq.map showElem elems)
          end
      | Arrow {from, arrow, to} =>
          showTy from ++ space ++ token arrow ++ space ++ showTy to
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

      | DecType {typbind={elems, delims}, typee} =>
          let
            (* val {tyvars, tycon, ty, ...} = Seq.nth elems 0 *)

            fun mk (delim, {tyvars, tycon, ty, eq}) =
              group (
                separateWithSpaces
                  [ SOME (token delim)
                  , maybeShowSyntaxSeq tyvars token
                  , SOME (token tycon)
                  , SOME (token eq)
                  ]
                $$
                (spaces 2 ++ showTy ty)
              )

            val first =
              let
                val {tyvars, tycon, ty, eq} = Seq.nth elems 0
              in
                group (
                  separateWithSpaces
                    [ SOME (token typee)
                    , maybeShowSyntaxSeq tyvars token
                    , SOME (token tycon)
                    , SOME (token eq)
                    ]
                  $$
                  (spaces 2 ++ showTy ty)
                )
              end
          in
            Seq.iterate op$$ first
              (Seq.map mk (Seq.zip (delims, Seq.drop elems 1)))
          end

      | DecDatatype {datbind = {elems, ...}, withtypee, ...} =>
          let
            fun showCon {opp, id, arg} =
              group (
                separateWithSpaces
                  [ Option.map (fn _ => text "op") opp
                  , SOME (text (Token.toString id))
                  , Option.map (fn {ty, ...} => text "of" ++ space ++ showTy ty) arg
                  ]
              )

            fun show_datbind mark {tyvars, tycon, elems, ...} =
              let
                val initial =
                  group (
                    separateWithSpaces
                      [ SOME (text (if mark then "datatype" else "and"))
                      , maybeShowSyntaxSeq tyvars (PD.text o Token.toString)
                      , SOME (text (Token.toString tycon))
                      , SOME (text "=")
                      ]
                  )
              in
                group (
                  initial
                  $$
                  (spaces 2 ++
                    group (
                      Seq.iterate
                        (fn (prev, next) => prev $$ text "|" ++ space ++ next)
                        (spaces 2 ++ showCon (Seq.nth elems 0))
                        (Seq.map showCon (Seq.drop elems 1))
                    )
                  )
                )
              end

            fun show_withtypee {withtypee, typbind = {elems, ...}} =
              let
                fun mk mark {tyvars, tycon, eq, ty} =
                  group (
                    separateWithSpaces
                      [ SOME (text (if mark then "withtype" else "and"))
                      , maybeShowSyntaxSeq tyvars (PD.text o Token.toString)
                      , SOME (text (Token.toString tycon))
                      , SOME (text "=")
                      , SOME (showTy ty)
                      ]
                  )
              in
                Seq.iterate op$$
                  (mk true (Seq.nth elems 0))
                  (Seq.map (mk false) (Seq.drop elems 1))
              end

            val datbinds =
              Seq.iterate op$$
                (show_datbind true (Seq.nth elems 0))
                (Seq.map (show_datbind false) (Seq.drop elems 1))
          in
            case withtypee of
              SOME result =>
                datbinds $$ show_withtypee result
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

      | DecException {elems, ...} =>
          let
            fun showExbind exbind =
              case exbind of
                ExnNew {opp, id, arg} =>
                  separateWithSpaces
                    [ Option.map (fn _ => text "op") opp
                    , SOME (text (Token.toString id))
                    , Option.map (fn {ty, ...} =>
                        text "of" ++ space ++ showTy ty) arg
                    ]
              | ExnReplicate {opp, left_id, right_id, ...} =>
                  separateWithSpaces
                    [ Option.map (fn _ => text "op") opp
                    , SOME (text (Token.toString left_id))
                    , SOME (text "=")
                    , SOME (text (Token.toString (MaybeLongToken.getToken right_id)))
                    ]

            fun mk (i, x) =
              (if i = 0 then text "exception" else text "and")
              ++ space ++ showExbind x
          in
            Seq.iterate op$$ empty (Seq.mapIdx mk elems)
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

      | DecAbstype {datbind = {elems, ...}, withtypee, dec, ...} =>
          (** TODO clean up: lots of copy-paste from DecDatatype *)
          let
            fun showCon {opp, id, arg} =
              group (
                separateWithSpaces
                  [ Option.map (fn _ => text "op") opp
                  , SOME (text (Token.toString id))
                  , Option.map (fn {ty, ...} => text "of" ++ space ++ showTy ty) arg
                  ]
              )

            fun show_datbind mark {tyvars, tycon, elems, ...} =
              let
                val initial =
                  group (
                    separateWithSpaces
                      [ SOME (text (if mark then "abstype" else "and"))
                      , maybeShowSyntaxSeq tyvars (PD.text o Token.toString)
                      , SOME (text (Token.toString tycon))
                      , SOME (text "=")
                      ]
                  )
              in
                group (
                  initial
                  $$
                  (spaces 2 ++
                    group (
                      Seq.iterate
                        (fn (prev, next) => prev $$ text "|" ++ space ++ next)
                        (spaces 2 ++ showCon (Seq.nth elems 0))
                        (Seq.map showCon (Seq.drop elems 1))
                    )
                  )
                )
              end

            fun show_withtypee {withtypee, typbind = {elems, ...}} =
              let
                fun mk mark {tyvars, tycon, eq, ty} =
                  group (
                    separateWithSpaces
                      [ SOME (text (if mark then "withtype" else "and"))
                      , maybeShowSyntaxSeq tyvars (PD.text o Token.toString)
                      , SOME (text (Token.toString tycon))
                      , SOME (text "=")
                      , SOME (showTy ty)
                      ]
                  )
              in
                Seq.iterate op$$
                  (mk true (Seq.nth elems 0))
                  (Seq.map (mk false) (Seq.drop elems 1))
              end

            val datbinds =
              Seq.iterate op$$
                (show_datbind true (Seq.nth elems 0))
                (Seq.map (show_datbind false) (Seq.drop elems 1))

            val bottom =
              group (
                text "with"
                $$
                (spaces 2 ++ showDec dec)
                $$
                text "end"
              )
          in
            group (
              case withtypee of
                SOME result =>
                  datbinds
                  $$
                  show_withtypee result
                  $$
                  bottom

              | _ => datbinds $$ bottom
            )
          end

      | _ => text "<dec>"
    end



  and showPat pat =
    let
      open Ast.Pat
    in
      case pat of
        Wild tok =>
          token tok
      | Const tok =>
          token tok
      | Unit {left, right} =>
          token left ++ token right
      | Ident {opp, id} =>
          separateWithSpaces
            [ Option.map token opp
            , SOME (token (MaybeLongToken.getToken id))
            ]
      | Parens {left, pat, right} =>
          token left ++ showPat pat ++ token right
      | Tuple {left, elems, delims, right} =>
          sequence left delims right (Seq.map showPat elems)
      | List {left, elems, delims, right} =>
          sequence left delims right (Seq.map showPat elems)
      | Record {left, elems, delims, right} =>
          let
            fun showPatRow patrow =
              case patrow of
                DotDotDot ddd => token ddd
              | LabEqPat {lab, eq, pat} =>
                  token lab ++ space ++ token eq
                  ++ space ++ showPat pat
              | LabAsPat {id, ty, aspat} =>
                  separateWithSpaces
                    [ SOME (token id)
                    , Option.map (fn {colon, ty} => token colon ++ space ++ showTy ty) ty
                    , Option.map (fn {ass, pat} => token ass ++ space ++ showPat pat) aspat
                    ]
          in
            sequence left delims right (Seq.map showPatRow elems)
          end
      | Con {opp, id, atpat} =>
          separateWithSpaces
            [ Option.map token opp
            , SOME (token (MaybeLongToken.getToken id))
            , SOME (showPat atpat)
            ]
      | Typed {pat, colon, ty} =>
          showPat pat ++ space ++ token colon ++ space ++ showTy ty
      | Layered {opp, id, ty, ass, pat} =>
          separateWithSpaces
            [ Option.map token opp
            , SOME (token id)
            , Option.map (fn {colon, ty} => token colon ++ space ++ showTy ty) ty
            , SOME (token ass)
            , SOME (showPat pat)
            ]
      | Infix {left, id, right} =>
          showPat left ++ space ++ token id ++ space ++ showPat right
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

      | MLtonSpecific {directive, contents, ...} =>
          text "<MLton-specific>"
    end



  fun showSpec spec =
    case spec of
      Ast.Sig.EmptySpec =>
        empty

    | Ast.Sig.Val {elems, ...} =>
        let
          fun showOne first {vid, ty, ...} =
            text (if first then "val" else "and")
            ++ space ++
            text (Token.toString vid) ++ space ++ text ":" ++ space
            ++ showTy ty
        in
          Seq.iterate op$$
            (showOne true (Seq.nth elems 0))
            (Seq.map (showOne false) (Seq.drop elems 1))
        end

    | Ast.Sig.Type {elems, ...} =>
        let
          fun showOne first {tyvars, tycon} =
            separateWithSpaces
              [ SOME (text (if first then "type" else "and"))
              , maybeShowSyntaxSeq tyvars (text o Token.toString)
              , SOME (text (Token.toString tycon))
              ]
        in
          Seq.iterate op$$
            (showOne true (Seq.nth elems 0))
            (Seq.map (showOne false) (Seq.drop elems 1))
        end

    | Ast.Sig.TypeAbbreviation {elems, ...} =>
        let
          fun showOne first {tyvars, tycon, ty, ...} =
            separateWithSpaces
              [ SOME (text (if first then "type" else "and"))
              , maybeShowSyntaxSeq tyvars (text o Token.toString)
              , SOME (text (Token.toString tycon))
              , SOME (text "=")
              , SOME (showTy ty)
              ]
        in
          Seq.iterate op$$
            (showOne true (Seq.nth elems 0))
            (Seq.map (showOne false) (Seq.drop elems 1))
        end

    | Ast.Sig.Eqtype {elems, ...} =>
        let
          fun showOne first {tyvars, tycon} =
            separateWithSpaces
              [ SOME (text (if first then "eqtype" else "and"))
              , maybeShowSyntaxSeq tyvars (text o Token.toString)
              , SOME (text (Token.toString tycon))
              ]
        in
          Seq.iterate op$$
            (showOne true (Seq.nth elems 0))
            (Seq.map (showOne false) (Seq.drop elems 1))
        end

    | Ast.Sig.Datatype {elems, ...} =>
        let
          fun showCon {vid, arg} =
              group (
                separateWithSpaces
                  [ SOME (text (Token.toString vid))
                  , Option.map (fn {ty, ...} => text "of" ++ space ++ showTy ty) arg
                  ]
              )

          fun show_datdesc mark {tyvars, tycon, elems, ...} =
            let
              val initial =
                group (
                  separateWithSpaces
                    [ SOME (text (if mark then "datatype" else "and"))
                    , maybeShowSyntaxSeq tyvars (PD.text o Token.toString)
                    , SOME (text (Token.toString tycon))
                    , SOME (text "=")
                    ]
                )
            in
              group (
                initial
                $$
                (spaces 2 ++
                  group (
                    Seq.iterate
                      (fn (prev, next) => prev $$ text "|" ++ space ++ next)
                      (spaces 2 ++ showCon (Seq.nth elems 0))
                      (Seq.map showCon (Seq.drop elems 1))
                  )
                )
              )
            end
        in
          Seq.iterate op$$
            (show_datdesc true (Seq.nth elems 0))
            (Seq.map (show_datdesc false) (Seq.drop elems 1))
        end

    | Ast.Sig.ReplicateDatatype {left_id, right_id, ...} =>
        group (
          separateWithSpaces
            [ SOME (text "datatype")
            , SOME (text (Token.toString left_id))
            , SOME (text "=")
            ]
          $$
          spaces 2 ++
            ( text "datatype"
              ++ space ++
              text (Token.toString (MaybeLongToken.getToken right_id))
            )
        )

    | Ast.Sig.Exception {elems, ...} =>
        let
          fun showOne first {vid, arg} =
              group (
                separateWithSpaces
                  [ SOME (text (if first then "exception" else "and"))
                  , SOME (text (Token.toString vid))
                  , Option.map (fn {ty, ...} => text "of" ++ space ++ showTy ty) arg
                  ]
              )
        in
          Seq.iterate op$$
            (showOne true (Seq.nth elems 0))
            (Seq.map (showOne false) (Seq.drop elems 1))
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

    (* | _ => text "<strexp>" *)


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

    | Ast.Str.MLtonOverload _ =>
        text "<_overload>"

    (* | _ => text "<strdec>" *)


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


  fun pretty (Ast.Ast tds) =
    if Seq.length tds = 0 then
      ""
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
        val doc = Seq.iterate op$$ (Seq.nth all 0) (Seq.drop all 1)
      in
        PD.toString doc
      end

end
