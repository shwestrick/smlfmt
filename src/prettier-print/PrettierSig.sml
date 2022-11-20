(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierSig:
sig
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab
  val showSpec: Ast.Sig.spec -> doc
  val showSigExp: Ast.Sig.sigexp -> doc
  val showSigExpAt: tab -> Ast.Sig.sigexp -> doc
  val showSigDec: Ast.Sig.sigdec -> doc
  val showSigDecAt: tab -> Ast.Sig.sigdec -> doc
end =
struct

  open TabbedTokenDoc
  open PrettierUtil

  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab

  fun showSpec _ = text "<spec>"
  fun showSigExp _ = text "<sigexp>"
  fun showSigExpAt _ _ = text "<sigexp>"
  fun showSigDec _ = text "<sigdec>"
  fun showSigDecAt _ _ = text "<sigdec>"

end
