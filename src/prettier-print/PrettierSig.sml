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
  infix 2 ++
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab

  fun showSpec tab spec =
    let
      open Ast.Sig
    in
      case spec of
        EmptySpec => empty
      | _ => text "<spec>"
    end

  fun showSigExp tab sigexp =
    let
      open Ast.Sig
    in
      case sigexp of
        Ident id =>
          token id

      | Spec {sigg, spec, endd} =>
          newTab tab (fn inner =>
            token sigg
            ++ at inner
            ++ showSpec inner spec
            ++ cond inner {inactive = empty, active = at tab}
            ++ token endd)

      | _ => text "<sigexp>"
    end

  fun showSigDec _ _ = text "<sigdec>"

end
