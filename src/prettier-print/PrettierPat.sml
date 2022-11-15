(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierPat:
sig
  val showPat: Ast.Pat.t -> TabbedStringDoc.tab TabbedTokenDoc.t
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  
  fun showPat _ = text "<pat>"
end
