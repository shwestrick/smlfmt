(** Copyright (c) 2020-2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettyStr:
sig
  val showStrExp: Ast.Str.strexp -> TokenDoc.t
  val showStrDec: Ast.Str.strdec -> TokenDoc.t
end =
struct

  open TokenDoc
  open PrettyUtil

  infix 2 ++ $$ //
  fun x ++ y = beside (x, y)
  fun x $$ y = aboveOrSpace (x, y)
  fun x // y = aboveOrBeside (x, y)

  fun showTy ty = PrettyTy.showTy ty
  fun showPat pat = PrettyPat.showPat pat
  fun showExp exp = PrettyExpAndDec.showExp exp
  fun showDec dec = PrettyExpAndDec.showDec dec
  fun showSpec spec = PrettySig.showSpec spec
  fun showSigExp sigexp = PrettySig.showSigExp sigexp
  fun showSigDec sigdec = PrettySig.showSigDec sigdec

  fun showStrExp e =
    case e of
      Ast.Str.Ident id =>
        token (MaybeLongToken.getToken id)

    | Ast.Str.Struct {structt, strdec, endd} =>
        group (
          token structt
          $$
          indent (showStrDec strdec)
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
            indent (prettyDec)
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
            indent (group (prettyExp))
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
              indent (showStrExp strexp)
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
            indent (showStrDec strdec1)
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
            indent (group (showStrDec strdec2))
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
            indent (
              Seq.iterate op$$
                (token (MaybeLongToken.getToken (Seq.nth elems 0)))
                (Seq.zipWith showOne (delims, Seq.drop elems 1))
            )
          )
        end

end
