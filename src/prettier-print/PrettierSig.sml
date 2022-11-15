(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierSig:
sig
  val showSpec: Ast.Sig.spec -> TabbedStringDoc.tab TabbedTokenDoc.t
  val showSigExp: Ast.Sig.sigexp -> TabbedStringDoc.tab TabbedTokenDoc.t
  val showSigDec: Ast.Sig.sigdec -> TabbedStringDoc.tab TabbedTokenDoc.t
end =
struct

  open TabbedTokenDoc
  open PrettierUtil

  fun showSpec _ = text "<spec>"
  fun showSigExp _ = text "<sigexp>"
  fun showSigDec _ = text "<sigdec>"

end
