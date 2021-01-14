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


  fun showSyntaxSeq s f =
    case s of
      Ast.SyntaxSeq.Empty => empty
    | Ast.SyntaxSeq.One x => f x
    | Ast.SyntaxSeq.Many {elems, ...} =>
        sequence "(" "," ")" (Seq.map f elems)


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
            (showSyntaxSeq args showTy ++ space
            ++ text (Token.toString (Ast.MaybeLong.getToken id)))
      | Parens {ty, ...} =>
          parensAround (showTy ty)
      | Tuple {elems, ...} =>
          let
            val begin = showTy (Seq.nth elems 0)
            fun f x = space ++ text "*" ++ space ++ showTy x
          in
            Seq.iterate op++ begin (Seq.map f (Seq.drop elems 1))
          end
      | Arrow {from, to, ...} =>
          (* parensAround *)
            (showTy from ++ space ++ text "->" ++ space ++ showTy to)
      | _ =>
        text "<ty>"
    end

  fun showDec dec =
    let
      open Ast.Exp
    in
      case dec of
        DecVal {vall, tyvars, elems, delims} =>
          let
            val {recc, pat, eq, exp} = Seq.nth elems 0
          in
            group (
              group (
                text "val" ++ space
                ++ showSyntaxSeq tyvars (PD.text o Token.toString) ++ space
                ++ (if Option.isSome recc then text "rec" else empty)
              )
              ++ space
              ++ showPat pat ++ space
              ++ text "="
              $$
              (spaces 2 ++ showExp exp)
            )
          end

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
        Atpat (Wild _) =>
          text "_"
      | Atpat (Const tok) =>
          text (Token.toString tok)
      | Atpat (Unit _) =>
          text "()"
      | Atpat (Ident {opp, id}) =>
          (if Option.isSome opp then text "op" else empty)
          ++ text (Token.toString (Ast.MaybeLong.getToken id))
      | Atpat (Parens {pat, ...}) =>
          parensAround (showPat pat)
      | Atpat (Tuple {elems, ...}) =>
          sequence "(" "," ")" (Seq.map showPat elems)
      | Typed {pat, ty, ...} =>
          showPat pat ++ space ++ text ":" ++ space ++ showTy ty
      | _ =>
          text "<pat>"
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
          (if Option.isSome opp then text "op" else empty)
          ++ text (Token.toString (Ast.MaybeLong.getToken id))
      | Parens {exp, ...} =>
          parensAround (showExp exp)
      | Tuple {elems, ...} =>
          sequence "(" "," ")" (Seq.map showExp elems)
      | Sequence {elems, ...} =>
          sequence "(" ";" ")" (Seq.map showExp elems)
      | App {left, right} =>
          group (showExp left $$ (spaces 2 ++ showExp right))
      | Infix {left, id, right} =>
          parensAround (group (
            showExp left ++ space ++ text (Token.toString id)
            $$
            showExp right
          ))
      | Typed {exp, ty, ...} =>
          showExp exp ++ space ++ text ":" ++ space ++ showTy ty
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
