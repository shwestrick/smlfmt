(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierStr:
sig
  val showStrExp: Ast.Str.strexp -> TabbedStringDoc.tab TabbedTokenDoc.t
  val showStrDec: Ast.Str.strdec -> TabbedStringDoc.tab TabbedTokenDoc.t
end =
struct
  
  open TabbedTokenDoc
  open PrettierUtil

  fun showStrExp _ = text "<strexp>"
  fun showStrDec _ = text "<strdec>"

end
