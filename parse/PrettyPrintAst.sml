(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettyPrintAst:
sig
  val pretty: Ast.t -> string
end =
struct

  structure PD = PrettierDoc

  infix 2 ++ +/+ +\+ +\\+ +-+
  fun x ++ y = PD.cat (x, y)

  fun x +/+ y = x ++ PD.softbreak ++ x

  fun x +\+ y = x ++ PD.softline ++ y
  fun x +\\+ y = x ++ PD.line ++ y


  val space = PD.text " "
  fun x +-+ y = x ++ space ++ y

  fun ind x = PD.nest 2 x

  fun parensAround (x: PD.t) =
    PD.text "(" ++ x ++ PD.text ")"

  fun sequence delim (xs: PD.t Seq.t) =
    let
      val n = Seq.length xs

      fun get i =
        delim ++ space ++ Seq.nth xs i
    in
      Seq.iterate op+/+ (Seq.nth xs 0) (Seq.tabulate (fn i => get (i+1)) (n-1))
    end


  fun pretty ast =
    let
      fun showSyntaxSeq s f =
        case s of
          Ast.SyntaxSeq.Empty => PD.empty
        | Ast.SyntaxSeq.One x => PD.text (f x)
        | Ast.SyntaxSeq.Many {elems, ...} =>
            parensAround (sequence (PD.text ",") (Seq.map (PD.text o f) elems))

      fun showDec dec =
        let
          open Ast.Exp
        in
          case dec of
            DecVal {vall, tyvars, elems, delims} =>
              let
                val {recc, pat, eq, exp} = Seq.nth elems 0
              in
                PD.group (
                  PD.text "val"
                  +-+ showSyntaxSeq tyvars Token.toString
                  +-+ (if Option.isSome recc then PD.text "rec" else PD.empty)
                  +-+ showPat pat
                  +-+ PD.text "=")
                +\+ showExp exp
              end

          | DecMultiple {elems, ...} =>
              let
                val elems = Seq.map showDec elems
              in
                Seq.iterate op+\\+ (Seq.nth elems 0) (Seq.drop elems 1)
              end

          | DecEmpty =>
              PD.empty

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
              (if Option.isSome opp then PD.text "op" else PD.empty)
              ++ PD.text (Token.toString (Ast.MaybeLong.getToken id))
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
              (if Option.isSome opp then PD.text "op" else PD.empty)
              ++ PD.text (Token.toString (Ast.MaybeLong.getToken id))
          | Parens {exp, ...} =>
              parensAround (showExp exp)
          | LetInEnd {dec, exps, ...} =>
              let
                val prettyDec = showDec dec
                val prettyExp = showExp (Seq.nth exps 0)
              in
                (** TODO: we need explicit vertical alignment to make this
                  * work. Is this what the `align` feature from wl-pprint is
                  * for??
                  *)
                PD.group (
                  PD.text "let"
                  +\\+
                  ind (PD.group prettyDec)
                  +\\+
                  PD.text "in"
                  +\\+
                  ind (PD.group prettyExp)
                  +\\+
                  PD.text "end"
                )
              end

          | _ =>
              PD.text "<exp>"
        end

    in
      case ast of Ast.Dec d => PrettierDoc.toString (showDec d)
    end

end
