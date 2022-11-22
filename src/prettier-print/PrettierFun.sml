(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierFun:
sig
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab
  val showFunDec: tab -> Ast.Fun.fundec -> doc
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  infix 2 ++
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab

  fun showFunDec _ _ = text "<fundec>"

end
