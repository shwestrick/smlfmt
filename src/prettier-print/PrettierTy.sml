(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierTy:
sig
  val showTy: Ast.Ty.t -> TabbedStringDoc.tab TabbedTokenDoc.t
end =
struct

  open TabbedTokenDoc
  open PrettierUtil

  fun showTy t = text "<ty>"

end
