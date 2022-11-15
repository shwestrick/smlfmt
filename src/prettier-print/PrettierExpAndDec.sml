(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierExpAndDec:
sig
  val showExp: Ast.Exp.exp -> TabbedStringDoc.tab TabbedTokenDoc.t
  val showDec: Ast.Exp.dec -> TabbedStringDoc.tab TabbedTokenDoc.t
end =
struct

  open TabbedTokenDoc
  open PrettierUtil

  fun showExp _ = text "<exp>"
  fun showDec _ = text "<dec>"

end
