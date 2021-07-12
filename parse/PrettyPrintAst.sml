(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettyPrintAst:
sig
  val pretty: Ast.t -> string
end =
struct

  structure PD = PrettySimpleDoc
  open PD

  infix 2 ++ $$ //
  fun x ++ y = beside (x, y)
  fun x $$ y = aboveOrSpace (x, y)
  fun x // y = aboveOrBeside (x, y)

  fun spaces n =
    List.foldl op++ empty (List.tabulate (n, fn _ => space))

  fun parensAround (x: doc) =
    text "(" ++ x ++ text ")"

  fun sequence openn delim close (xs: PD.t Seq.t) =
    if Seq.length xs = 0 then
      text openn ++ text close
    else
      let
        val top = text openn ++ softspace ++ Seq.nth xs 0
        fun f x = text delim ++ space ++ x
      in
        group (
          Seq.iterate op// top (Seq.map f (Seq.drop xs 1))
          //
          text close
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
    | Ast.SyntaxSeq.Many {elems, ...} =>
        SOME (sequence "(" "," ")" (Seq.map f elems))


  fun showTy ty =
    let
      open Ast.Ty
    in
      case ty of
        Var tok =>
          text (Token.toString tok)
      | Con {args = Ast.SyntaxSeq.Empty, id} =>
          (* text "CON" ++ parensAround *)
            (text (Token.toString (Ast.MaybeLong.getToken id)))
      | Con {args, id} =>
          (* text "CON" ++ parensAround *)
          (separateWithSpaces
            [ maybeShowSyntaxSeq args showTy
            , SOME (text (Token.toString (Ast.MaybeLong.getToken id)))
            ])
      | Parens {ty, ...} =>
          parensAround (showTy ty)
      | Tuple {elems, ...} =>
          let
            val begin = showTy (Seq.nth elems 0)
            fun f x = space ++ text "*" ++ space ++ showTy x
          in
            Seq.iterate op++ begin (Seq.map f (Seq.drop elems 1))
          end
      | Record {elems, ...} =>
          let
            fun showElem {lab, ty, ...} =
              text (Token.toString lab) ++ space ++ text ":"
              ++ space ++ showTy ty
          in
            sequence "{" "," "}" (Seq.map showElem elems)
          end
      | Arrow {from, to, ...} =>
          (* parensAround *)
            (showTy from ++ space ++ text "->" ++ space ++ showTy to)
    end

  fun showDec dec =
    let
      open Ast.Exp
    in
      case dec of
        DecVal {vall, tyvars, elems, delims} =>
          let
            fun mk {recc, pat, eq, exp} =
              group (
                separateWithSpaces
                  [ SOME (text "and")
                  , Option.map (fn _ => text "rec") recc
                  , SOME (showPat pat)
                  , SOME (text "=")
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
                    [ SOME (text "val")
                    , maybeShowSyntaxSeq tyvars (PD.text o Token.toString)
                    , Option.map (fn _ => text "rec") recc
                    , SOME (showPat pat)
                    , SOME (text "=")
                    ]
                  $$
                  (spaces 2 ++ showExp exp)
                )
              end
          in
            Seq.iterate op$$ first (Seq.map mk (Seq.drop elems 1))
          end

      | DecFun {funn, tyvars, fvalbind={elems, ...}} =>
          let
            fun mk {elems=innerElems, delims} =
              let
                val {opp, id, args, ty, eq, exp} = Seq.nth innerElems 0
                val prettyArgs =
                  Seq.iterate (fn (prev, p) => prev ++ space ++ showPat p)
                    (showPat (Seq.nth args 0))
                    (Seq.drop args 1)
              in
                group (
                  separateWithSpaces
                    [ SOME (text "and")
                    , Option.map (fn _ => text "op") opp
                    , SOME (text (Token.toString id))
                    , SOME prettyArgs
                    , Option.map (fn {ty, ...} => text ":" ++ space ++ showTy ty) ty
                    , SOME (text "=")
                    ]
                  $$
                  (spaces 2 ++ showExp exp)
                )
              end


            val first =
              let
                val {elems=innerElems, ...} = Seq.nth elems 0
                val {opp, id, args, ty, eq, exp} = Seq.nth innerElems 0
                val prettyArgs =
                  Seq.iterate (fn (prev, p) => prev ++ space ++ showPat p)
                    (showPat (Seq.nth args 0))
                    (Seq.drop args 1)
              in
                group (
                  separateWithSpaces
                    [ SOME (text "fun")
                    , maybeShowSyntaxSeq tyvars (PD.text o Token.toString)
                    , Option.map (fn _ => text "op") opp
                    , SOME (text (Token.toString id))
                    , SOME prettyArgs
                    , Option.map (fn {ty, ...} => text ":" ++ space ++ showTy ty) ty
                    , SOME (text "=")
                    ]
                  $$
                  (spaces 2 ++ showExp exp)
                )
              end
          in
            Seq.iterate op$$ first (Seq.map mk (Seq.drop elems 1))
          end

      | DecType {typbind={elems, ...}, ...} =>
          let
            val {tyvars, tycon, ty, ...} = Seq.nth elems 0

            fun mk {tyvars, tycon, ty, ...} =
              group (
                separateWithSpaces
                  [ SOME (text "and")
                  , maybeShowSyntaxSeq tyvars (PD.text o Token.toString)
                  , SOME (text (Token.toString tycon))
                  , SOME (text "=")
                  ]
                $$
                (spaces 2 ++ showTy ty)
              )

            val first =
              let
                val {tyvars, tycon, ty, ...} = Seq.nth elems 0
              in
                group (
                  separateWithSpaces
                    [ SOME (text "type")
                    , maybeShowSyntaxSeq tyvars (PD.text o Token.toString)
                    , SOME (text (Token.toString tycon))
                    , SOME (text "=")
                    ]
                  $$
                  (spaces 2 ++ showTy ty)
                )
              end
          in
            Seq.iterate op$$ first (Seq.map mk (Seq.drop elems 1))
          end

      | DecInfix {precedence, elems, ...} =>
          let
            val ids =
              Seq.iterate
                (fn (prev, id) => prev ++ space ++ text (Token.toString id))
                (text (Token.toString (Seq.nth elems 0)))
                (Seq.drop elems 1)
          in
            separateWithSpaces
              [ SOME (text "infix")
              , Option.map (text o Token.toString) precedence
              , SOME ids
              ]
          end

      | DecInfixr {precedence, elems, ...} =>
          let
            val ids =
              Seq.iterate
                (fn (prev, id) => prev ++ space ++ text (Token.toString id))
                (text (Token.toString (Seq.nth elems 0)))
                (Seq.drop elems 1)
          in
            separateWithSpaces
              [ SOME (text "infix")
              , Option.map (text o Token.toString) precedence
              , SOME ids
              ]
          end

      | DecNonfix {elems, ...} =>
          let
            val ids =
              Seq.iterate
                (fn (prev, id) => prev ++ space ++ text (Token.toString id))
                (text (Token.toString (Seq.nth elems 0)))
                (Seq.drop elems 1)
          in
            text "nonfix" ++ space ++ ids
          end

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
                    , SOME (text (Token.toString (Ast.MaybeLong.getToken right_id)))
                    ]

            fun mk (i, x) =
              (if i = 0 then text "exception" else text "and")
              ++ space ++ showExbind x
          in
            Seq.iterate op$$ empty (Seq.mapIdx mk elems)
          end

      | DecLocal {left_dec, right_dec, ...} =>
          group (
            text "local"
            $$
            (spaces 2 ++ showDec left_dec)
            $$
            text "in"
            $$
            (spaces 2 ++ showDec right_dec)
            $$
            text "end"
          )

      | DecMultiple {elems, delims} =>
          let
            fun f i =
              showDec (Seq.nth elems i)
              ++
              (if Option.isSome (Seq.nth delims i) then text ";" else empty)
          in
            Util.loop (0, Seq.length elems) empty (fn (prev, i) => prev $$ f i)
          end

      | DecEmpty =>
          empty

      | _ =>
          text "<dec>"
    end



  and showPat pat =
    let
      open Ast.Pat
    in
      case pat of
        Wild _ =>
          text "_"
      | Const tok =>
          text (Token.toString tok)
      | Unit _ =>
          text "()"
      | Ident {opp, id} =>
          (if Option.isSome opp then text "op " else empty)
          ++ text (Token.toString (Ast.MaybeLong.getToken id))
      | Parens {pat, ...} =>
          parensAround (showPat pat)
      | Tuple {elems, ...} =>
          sequence "(" "," ")" (Seq.map showPat elems)
      | List {elems, ...} =>
          sequence "[" "," "]" (Seq.map showPat elems)
      | Record {elems, ...} =>
          let
            fun showPatRow patrow =
              case patrow of
                DotDotDot _ => text "..."
              | LabEqPat {lab, pat, ...} =>
                  text (Token.toString lab) ++ space ++ text "="
                  ++ space ++ showPat pat
              | LabAsPat {id, ty, aspat} =>
                  separateWithSpaces
                    [ SOME (text (Token.toString id))
                    , Option.map (fn {ty, ...} => text ":" ++ space ++ showTy ty) ty
                    , Option.map (fn {pat, ...} => text "as" ++ space ++ showPat pat) aspat
                    ]
          in
            sequence "{" "," "}" (Seq.map showPatRow elems)
          end
      | Con {opp, id, atpat} =>
          (if Option.isSome opp then text "op " else empty)
          ++ text (Token.toString (Ast.MaybeLong.getToken id))
          ++ space ++ showPat atpat
      | Typed {pat, ty, ...} =>
          showPat pat ++ space ++ text ":" ++ space ++ showTy ty
      | Layered {opp, id, ty, pat, ...} =>
          separateWithSpaces
            [ Option.map (fn _ => text "op") opp
            , SOME (text (Token.toString id))
            , Option.map (fn {ty, ...} => text ":" ++ space ++ showTy ty) ty
            , SOME (text "as")
            , SOME (showPat pat)
            ]
      | Infix {left, id, right} =>
          parensAround (group (
            showPat left ++ space ++ text (Token.toString id)
            $$
            showPat right
          ))
    end


  and showExp exp =
    let
      open Ast.Exp
    in
      case exp of
        Const tok =>
          text (Token.toString tok)
      | Unit _ =>
          text "()"
      | Ident {opp, id} =>
          (if Option.isSome opp then text "op " else empty)
          ++ text (Token.toString (Ast.MaybeLong.getToken id))
      | Parens {exp, ...} =>
          parensAround (showExp exp)
      | Tuple {elems, ...} =>
          sequence "(" "," ")" (Seq.map showExp elems)
      | Sequence {elems, ...} =>
          sequence "(" ";" ")" (Seq.map showExp elems)
      | List {elems, ...} =>
          sequence "[" "," "]" (Seq.map showExp elems)
      | Record {elems, ...} =>
          let
            fun showRow {lab, exp, ...} =
              group (
                (text (Token.toString lab) ++ space ++ text "=")
                $$
                (spaces 2 ++ showExp exp)
              )
          in
            sequence "{" "," "}" (Seq.map showRow elems)
          end
      | Select {label, ...} =>
          text "#" ++ space ++ text (Token.toString label)
      | App {left, right} =>
          group (showExp left $$ (spaces 2 ++ showExp right))
      | Infix {left, id, right} =>
          parensAround (group (
            showExp left ++ space ++ text (Token.toString id)
            $$
            showExp right
          ))
      | Andalso {left, right, ...} =>
          parensAround (group (
            showExp left ++ space ++ text "andalso"
            $$
            showExp right
          ))
      | Orelse {left, right, ...} =>
          parensAround (group (
            showExp left ++ space ++ text "orelse"
            $$
            showExp right
          ))
      | Typed {exp, ty, ...} =>
          showExp exp ++ space ++ text ":" ++ space ++ showTy ty

      | IfThenElse {exp1, exp2, exp3, ...} =>
          group (
            (text "if" ++ space ++ showExp exp1 ++ space ++ text "then")
            $$
            (spaces 2 ++ showExp exp2)
            $$
            text "else"
            $$
            (spaces 2 ++ showExp exp3)
          )

      | Raise {exp, ...} =>
          group (text "raise" $$ (spaces 2 ++ showExp exp))

      | Handle {exp=expLeft, elems, ...} =>
          let
            val first = Seq.nth elems 0
            val rest = Seq.drop elems 1

            fun mk {pat, exp, ...} =
              group (
                (text "|" ++ space ++ showPat pat ++ space ++ text "=>")
                $$
                (spaces 4 ++ showExp exp)
              )

            val {pat, exp, ...} = first
            val initial =
              group (
                showExp expLeft
                $$
                group (
                  text "handle"
                  $$
                  (spaces 2 ++ showPat pat ++ space ++ text "=>")
                  $$
                  (spaces 4 ++ showExp exp)
                )
              )

            val stuff =
              group (Seq.iterate (fn (prev, next) => prev $$ mk next) initial rest)
          in
            parensAround stuff
          end

      | Case {exp=expTop, elems, ...} =>
          let
            val first = Seq.nth elems 0
            val rest = Seq.drop elems 1

            fun mk {pat, exp, ...} =
              group (
                (text "|" ++ space ++ showPat pat ++ space ++ text "=>")
                $$
                (spaces 4 ++ showExp exp)
              )

            val {pat, exp, ...} = first
            val initial =
              (text "case" ++ space ++
              showExp expTop ++ space ++ text "of")
              $$
              (spaces 2 ++
                group (
                  (showPat pat ++ space ++ text "=>")
                  $$
                  (spaces 2 ++ showExp exp)
                )
              )

            val stuff =
              group (Seq.iterate (fn (prev, next) => prev $$ mk next) initial rest)
          in
            parensAround stuff
          end

      | Fn {fnn, elems, ...} =>
          let
            val first = Seq.nth elems 0
            val rest = Seq.drop elems 1

            fun mk {pat, exp, ...} =
              space ++
              group (
                (text "|" ++ space ++ showPat pat ++ space ++ text "=>")
                $$
                (spaces 4 ++ showExp exp)
              )

            val {pat, exp, ...} = first
            val initial =
              group (
                (text "fn" ++ space ++
                showPat pat ++ space ++
                text "=>")
                $$
                (spaces 4 ++ showExp exp)
              )
          in
            group (Seq.iterate (fn (prev, next) => prev $$ mk next) initial rest)
          end

      | LetInEnd {dec, exps, ...} =>
          let
            val prettyDec = showDec dec
            val numExps = Seq.length exps

            val withDelims = Seq.mapIdx (fn (i, e) =>
                showExp e ++ (if i = numExps - 1 then empty else text ";"))
              exps

            val topPart =
              text "let"
              $$
              (spaces 2 ++ prettyDec)
              $$
              text "in"

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
              text "end"
            )
          end

      | _ =>
          text "<exp>"
    end


  fun pretty ast =
    case ast of Ast.Dec d => PrettySimpleDoc.toString (showDec d)

end
