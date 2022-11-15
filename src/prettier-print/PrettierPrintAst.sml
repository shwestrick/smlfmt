(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierPrintAst:
sig
  val pretty: {ribbonFrac: real, maxWidth: int, tabWidth: int, indent: int}
           -> Ast.t
           -> TerminalColorString.t
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  infix 2 ++

  fun showTy ty = PrettierTy.showTy ty
  fun showPat pat = PrettierPat.showPat pat
  fun showExp exp = PrettierExpAndDec.showExp exp
  fun showDec dec = PrettierExpAndDec.showDec dec
  fun showSpec spec = PrettierSig.showSpec spec
  fun showSigExp sigexp = PrettierSig.showSigExp sigexp
  fun showSigDec sigdec = PrettierSig.showSigDec sigdec
  fun showStrExp strexp = PrettierStr.showStrExp strexp
  fun showStrDec strdec = PrettierStr.showStrDec strdec
  fun showFunDec fundec = PrettierFun.showFunDec fundec

  fun showAst (Ast.Ast tds) =
    if Seq.length tds = 0 then
      empty
    else
      let
        fun showOne tab {topdec, semicolon} =
          let
            val td =
              case topdec of
                Ast.StrDec d => break tab ++ showStrDec d
              | Ast.SigDec d => break tab ++ showSigDec d
              | Ast.FunDec d => break tab ++ showFunDec d
            val sc =
              case semicolon of
                NONE => empty
              | SOME sc => token sc
          in
            td ++ sc
          end

      in
        newTab (fn tab =>
          let
            val all = Seq.map (showOne tab) tds
          in
            Seq.iterate op++ (Seq.nth all 0) (Seq.drop all 1)
          end)
      end


  fun pretty (params as {ribbonFrac, maxWidth, tabWidth, indent}) ast =
    let
      val doc = showAst ast
      (* val doc = TokenDoc.insertComments doc
      val doc = TokenDoc.insertBlankLines doc *)
    in
      TabbedStringDoc.pretty
        {ribbonFrac=ribbonFrac, maxWidth=maxWidth, indentWidth=indent}
        (toStringDoc doc)
    end

end
