(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierSig:
sig
  val showSpec: Ast.Sig.spec PrettierUtil.shower
  val showSigExp: Ast.Sig.sigexp PrettierUtil.shower
  val showSigDec: Ast.Sig.sigdec PrettierUtil.shower
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  open PrettierTy
  infix 2 ++
  fun x ++ y = concat (x, y)

  (* ======================================================================= *)

  fun leftMostSigExp e =
    let
      open Ast.Sig
    in
      case e of
        WhereType {sigexp, ...} => leftMostSigExp sigexp
      | _ => e
    end

  fun sigExpWantsSameTabAsDec e =
    let
      open Ast.Sig
    in
      case leftMostSigExp e of
        Ident _ => false
      | _ => true
    end

  (* ======================================================================= *)

  fun showSpec tab spec =
    let
      open Ast.Sig
    in
      case spec of
        EmptySpec => empty

      | Val {vall, elems, delims} =>
          let
            fun showOne first (starter, {vid, colon, ty}) =
              (if first then empty else at tab)
              ++ token starter ++ token vid ++ token colon ++ withNewChild showTy tab ty
          in
            Seq.iterate op++
              (showOne true (vall, Seq.nth elems 0))
              (Seq.zipWith (showOne false) (delims, Seq.drop elems 1))
          end

      | Type {typee, elems, delims} =>
          let
            fun showOne first (starter, {tyvars, tycon}) =
              (if first then empty else at tab)
              ++ token starter
              ++ showSyntaxSeq token tab tyvars
              ++ token tycon
          in
            Seq.iterate op++
              (showOne true (typee, Seq.nth elems 0))
              (Seq.zipWith (showOne false) (delims, Seq.drop elems 1))
          end

      | TypeAbbreviation {typee, elems, delims} =>
          let
            fun showOne first (starter, {tyvars, tycon, eq, ty}) =
              (if first then empty else at tab)
              ++ token starter
              ++ showSyntaxSeq token tab tyvars
              ++ token tycon
              ++ token eq
              ++ withNewChild showTy tab ty
          in
            Seq.iterate op++
              (showOne true (typee, Seq.nth elems 0))
              (Seq.zipWith (showOne false) (delims, Seq.drop elems 1))
          end

      | Multiple {elems, delims} =>
          let
            fun showOne first (elem: spec, delim: Token.t option) =
              (if first then empty else at tab)
              ++ showSpec tab elem
              ++ showOption (fn d => nospace ++ token d) delim

            val things = Seq.zip (elems, delims)
          in
            Seq.iterate op++
              (showOne true (Seq.nth things 0))
              (Seq.map (showOne false) (Seq.drop things 1))
          end

      | Exception {exceptionn, elems, delims} =>
          let
            fun showOne first (starter, {vid, arg}) =
              (if first then empty else at tab)
              ++ token starter
              ++ token vid
              ++ showOption (fn {off, ty} => token off ++ withNewChild showTy tab ty) arg
          in
            Seq.iterate op++
              (showOne true (exceptionn, Seq.nth elems 0))
              (Seq.zipWith (showOne false) (delims, Seq.drop elems 1))
          end

      | Structure {structuree, elems, delims} =>
          let
            fun showOne first (starter, {id, colon, sigexp}) =
              (if first then empty else at tab)
              ++ token starter
              ++ token id
              ++ token colon
              ++ (if sigExpWantsSameTabAsDec sigexp then
                    at tab ++ showSigExp tab sigexp
                  else
                    withNewChild showSigExp tab sigexp)
          in
            Seq.iterate op++
              (showOne true (structuree, Seq.nth elems 0))
              (Seq.zipWith (showOne false) (delims, Seq.drop elems 1))
          end

      | Include {includee, sigexp} =>
          token includee ++ withNewChild showSigExp tab sigexp

      | IncludeIds {includee, sigids} =>
          Seq.iterate op++
            (token includee)
            (Seq.map (withNewChild (fn _ => token) tab) sigids)

      | Sharing {spec, sharingg, elems, delims} =>
          newTab tab (fn inner =>
            let
              fun showOne (delim, elem) =
                at inner
                ++ token delim (** this is an '=' *)
                ++ token (MaybeLongToken.getToken elem)

              val first =
                at inner ++ token (MaybeLongToken.getToken (Seq.nth elems 0))

              val stuff =
                Seq.iterate op++
                  first
                  (Seq.zipWith showOne (delims, Seq.drop elems 1))
            in
              showSpec tab spec
              ++ at tab
              ++ token sharingg
              ++ stuff
            end)

      | SharingType {spec, sharingg, typee, elems, delims} =>
          newTab tab (fn inner =>
            let
              fun showOne (delim, elem) =
                at inner
                ++ token delim (** this is an '=' *)
                ++ token (MaybeLongToken.getToken elem)

              val first =
                at inner ++ token (MaybeLongToken.getToken (Seq.nth elems 0))

              val stuff =
                Seq.iterate op++
                  first
                  (Seq.zipWith showOne (delims, Seq.drop elems 1))
            in
              showSpec tab spec
              ++ at tab
              ++ token sharingg
              ++ token typee
              ++ stuff
            end)

      (*
      | Datatype _
      | Eqtype _
      | ReplicateDatatype _
      *)

      | _ => text "<spec>"
    end


  and showSigExp tab sigexp =
    let
      open Ast.Sig
    in
      case sigexp of
        Ident id =>
          token id

      | Spec {sigg, spec, endd} =>
          newTabWithStyle tab (Indented, fn inner =>
            token sigg
            ++ at inner
            ++ showSpec inner spec
            ++ cond inner {inactive = empty, active = at tab}
            ++ token endd)

      | WhereType {sigexp, elems} =>
          newTab tab (fn inner =>
            let
              fun showElem {wheree, typee, tyvars, tycon, eq, ty} =
                at inner
                ++ token wheree (** this could be 'and' *)
                ++ token typee
                ++ showSyntaxSeq token inner tyvars
                ++ token (MaybeLongToken.getToken tycon)
                ++ token eq
                ++ withNewChild showTy inner ty
            in
              Seq.iterate op++
                (showSigExp tab sigexp)
                (Seq.map showElem elems)
            end)
    end


  and showSigDec tab (Ast.Sig.Signature {signaturee, elems, delims}) =
    let
      fun showOne first (starter, {ident, eq, sigexp}) =
        (if first then empty else at tab)
        ++ token starter
        ++ token ident
        ++ token eq
        ++ (if sigExpWantsSameTabAsDec sigexp then
              at tab ++ showSigExp tab sigexp
            else
              withNewChild showSigExp tab sigexp)
    in
      Seq.iterate op++
        (showOne true (signaturee, Seq.nth elems 0))
        (Seq.zipWith (showOne false) (delims, Seq.drop elems 1))
    end

end
