(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierExpAndDec:
sig
  type doc = TabbedStringDoc.tab TabbedTokenDoc.t
  type tab = TabbedStringDoc.tab TabbedTokenDoc.tab
  val showExp: Ast.Exp.exp -> doc
  val showExpAt: tab -> Ast.Exp.exp -> doc
  val showDec: Ast.Exp.dec -> doc
  val showDecAt: tab -> Ast.Exp.dec -> doc
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  infix 2 ++

  type doc = TabbedStringDoc.tab TabbedTokenDoc.t
  type tab = TabbedStringDoc.tab TabbedTokenDoc.tab

  fun showExp _ = text "<exp>"
  fun showExpAt _ _ = text "<exp>"
  fun showDec _ = text "<dec>"
  fun showDecAt _ _ = text "<dec>"

end
