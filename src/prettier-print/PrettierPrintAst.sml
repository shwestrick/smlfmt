(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierPrintAst:
sig
  val pretty: {ribbonFrac: real, maxWidth: int, tabWidth: int, indent: int, debug: bool}
           -> Ast.t
           -> TerminalColorString.t
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  infix 2 ++

  fun showTy ty = PrettierTy.showTy ty
  fun showPat pat = PrettierPat.showPat pat
  fun showSpec tab spec = PrettierSig.showSpec tab spec
  fun showSigExp tab sigexp = PrettierSig.showSigExp tab sigexp
  fun showSigDec tab sigdec = PrettierSig.showSigDec tab sigdec
  fun showSigDec tab strdec = PrettierSig.showSigDec tab strdec
  fun showStrDec tab strdec = PrettierStr.showStrDec tab strdec
  fun showFunDec tab strdec = PrettierFun.showFunDec tab strdec

  (* ====================================================================== *)

  fun showAst (Ast.Ast tds) =
    if Seq.length tds = 0 then
      empty
    else
      let
        fun showOneAt tab {topdec, semicolon} =
          let
            val td =
              case topdec of
                Ast.StrDec d => showStrDec tab d
              | Ast.SigDec d => showSigDec tab d
              | Ast.FunDec d => showFunDec tab d
            val sc =
              case semicolon of
                NONE => empty
              | SOME sc => token sc
          in
            td ++ sc
          end

        val all = Seq.map (showOneAt root) tds
      in
        at root ++
        Seq.iterate op++ (Seq.nth all 0)
          (Seq.map (fn x => at root ++ x) (Seq.drop all 1))
      end


  fun pretty (params as {ribbonFrac, maxWidth, tabWidth, indent, debug}) ast =
    let
      val doc = showAst ast
      (* val doc = TokenDoc.insertComments doc
      val doc = TokenDoc.insertBlankLines doc *)
    in
      TabbedStringDoc.pretty
        {ribbonFrac=ribbonFrac, maxWidth=maxWidth, indentWidth=indent, debug=debug}
        (toStringDoc {tabWidth=tabWidth, debug=debug} doc)
    end

end
