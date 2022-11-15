(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierFun:
sig
  type doc = TabbedStringDoc.tab TabbedTokenDoc.t
  type tab = TabbedStringDoc.tab TabbedTokenDoc.tab
  val showFunDec: Ast.Fun.fundec -> doc
  val showFunDecAt: tab -> Ast.Fun.fundec -> doc
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  infix 2 ++
  type doc = TabbedStringDoc.tab TabbedTokenDoc.t
  type tab = TabbedStringDoc.tab TabbedTokenDoc.tab

  fun showFunDec _ = text "<fundec>"
  fun showFunDecAt _ _ = text "<fundec>"

end
