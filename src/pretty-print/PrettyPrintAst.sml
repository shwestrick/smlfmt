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
  fun showExp exp = PrettyExpAndDec.showExp exp
  fun showDec dec = PrettyExpAndDec.showDec dec
  fun showSpec spec = PrettySig.showSpec spec
  fun showSigExp sigexp = PrettySig.showSigExp sigexp
  fun showSigDec sigdec = PrettySig.showSigDec sigdec
  fun showStrExp strexp = PrettyStr.showStrExp strexp
  fun showStrDec strdec = PrettyStr.showStrDec strdec

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
