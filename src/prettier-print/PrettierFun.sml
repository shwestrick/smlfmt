(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierFun:
sig
  val showFunDec: Ast.Fun.fundec -> TabbedStringDoc.tab TabbedTokenDoc.t
end =
struct

  open TabbedTokenDoc
  open PrettierUtil

  fun showFunDec _ = text "<fundec>"

end
