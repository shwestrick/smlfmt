(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettyPrintAst:
sig
  val pretty: Ast.t -> string
end =
struct

  structure PD = PrettyDoc

  infix 2 ^^ ++ +/+ +/+? ?+/+
  fun x ^^ y = PD.verticalNest (x, 0, y)
  fun x ++ y = PD.horizontal (x, y)
  fun x +/+ y = PD.separate (x, y)

  fun x +/+? NONE = x
    | x +/+? (SOME y) = x +/+ y

  fun NONE ?+/+ y = y
    | (SOME x) ?+/+ y = x +/+ y

  fun parensAround (x: PD.t) =
    PD.text "(" ++ x ++ PD.text ")"

  fun sequence delim (xs: PD.t Seq.t) =
    let
      val n = Seq.length xs

      fun get i =
        PD.text "," +/+ Seq.nth xs i
    in
      Seq.iterate op++ (Seq.nth xs 0) (Seq.tabulate (fn i => get (i+1)) (n-1))
    end


  fun pretty ast =
    let
      fun maybeShowSyntaxSeq s f =
        case s of
          Ast.SyntaxSeq.Empty => NONE
        | Ast.SyntaxSeq.One x => SOME (PD.text (f x))
        | Ast.SyntaxSeq.Many {elems, ...} =>
            SOME (parensAround (sequence (PD.text ",") (Seq.map (PD.text o f) elems)))

      fun showDec dec =
        let
          open Ast.Exp
        in
          case dec of
            DecVal {vall, tyvars, elems, delims} =>
              let
                val {recc, pat, eq, exp} = Seq.nth elems 0
              in
                PD.text "val"
                +/+? maybeShowSyntaxSeq tyvars Token.toString
                +/+? Option.map (fn _ => PD.text "rec") recc
                +/+ showPat pat
                +/+ PD.text "="
                +/+ showExp exp
              end

          | DecMultiple {elems, ...} =>
              let
                val elems = Seq.map showDec elems
              in
                Seq.iterate op^^ (Seq.nth elems 0) (Seq.drop elems 1)
              end

          | DecEmpty =>
              PD.text ""

          | _ =>
              PD.text "<dec>"
        end

      and showPat pat =
        let
          open Ast.Pat
        in
          case pat of
            Atpat (Wild _) =>
              PD.text "_"
          | Atpat (Const tok) =>
              PD.text (Token.toString tok)
          | Atpat (Unit _) =>
              PD.text "()"
          | Atpat (Ident {opp, id}) =>
              Option.map (fn _ => PD.text "op") opp
              ?+/+ PD.text (Token.toString (Ast.MaybeLong.getToken id))
          | Atpat (Parens {pat, ...}) =>
              parensAround (showPat pat)
          | _ =>
              PD.text "<pat>"
        end


      and showExp exp =
        let
          open Ast.Exp
        in
          case exp of
            Const tok =>
              PD.text (Token.toString tok)
          | Unit _ =>
              PD.text "()"
          | Ident {opp, id} =>
              Option.map (fn _ => PD.text "op") opp
              ?+/+ PD.text (Token.toString (Ast.MaybeLong.getToken id))
          | Parens {exp, ...} =>
              parensAround (showExp exp)
          | LetInEnd {dec, exps, ...} =>
              let
                val prettyDec = showDec dec
                val prettyExp = showExp (Seq.nth exps 0)
              in
                PD.verticalNest (PD.text "let", 2, prettyDec)
                ^^
                PD.verticalNest (PD.text "in", 2, prettyExp)
                ^^
                PD.text "end"
              end
          | _ =>
              PD.text "<exp>"
        end

    in
      case ast of Ast.Dec d => PrettyDoc.pretty (showDec d)
    end

end
