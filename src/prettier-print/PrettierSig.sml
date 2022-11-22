(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierSig:
sig
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab
  val showSpec: tab -> Ast.Sig.spec -> doc
  val showSigExp: tab -> Ast.Sig.sigexp -> doc
  val showSigDec: tab -> Ast.Sig.sigdec -> doc
end =
struct

  open TabbedTokenDoc
  open PrettierUtil

  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab

  fun showSpec _ _ = text "<spec>"
  fun showSigExp _ _ = text "<sigexp>"
  fun showSigDec _ _ = text "<sigdec>"

end
