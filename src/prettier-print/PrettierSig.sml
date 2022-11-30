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

  (* NOTE: very similar to PrettierExpAndDec.showDatbind. The
   * only difference: there is no possible 'op' in the condesc, ugh.
   *)
  fun showDatspec tab {datatypee, elems, delims} =
    newTab tab (fn tab =>
    let
      fun showCon (starter, {vid, arg}) =
        at tab ++ starter ++
        token vid ++
        showOption (fn {off, ty} => token off ++ withNewChild showTy tab ty) arg

      fun showOne (starter, {tyvars, tycon, eq, elems, delims}) =
        let
          val initial =
            at tab ++
            token starter ++
            showSyntaxSeq token tab tyvars ++
            token tycon ++
            token eq

          val skipper = cond tab {inactive=empty, active=space++space}
          fun dd delim = token delim ++ space
        in
          initial ++
          Seq.iterate op++
            (showCon (skipper, Seq.nth elems 0))
            (Seq.zipWith showCon (Seq.map dd delims, Seq.drop elems 1))
        end
    in
      Seq.iterate op++
        (showOne (datatypee, Seq.nth elems 0))
        (Seq.zipWith showOne (delims, Seq.drop elems 1))
    end)

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

      | Eqtype {eqtypee, elems, delims} =>
          let
            fun showOne first (starter, {tyvars, tycon}) =
              (if first then empty else at tab)
              ++ token starter
              ++ showSyntaxSeq token tab tyvars
              ++ token tycon
          in
            Seq.iterate op++
              (showOne true (eqtypee, Seq.nth elems 0))
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

      | ReplicateDatatype
        {left_datatypee, left_id, eq, right_datatypee, right_id} =>
          token left_datatypee
          ++ token left_id
          ++ token eq
          ++ newTab tab (fn inner =>
              at inner
              ++ token right_datatypee
              ++ token (MaybeLongToken.getToken right_id))

      | Datatype xxx =>
          showDatspec tab xxx
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
          let
            fun showElem {wheree, typee, tyvars, tycon, eq, ty} =
              at tab
              ++ token wheree (** this could be 'and' *)
              ++ token typee
              ++ showSyntaxSeq token tab tyvars
              ++ token (MaybeLongToken.getToken tycon)
              ++ token eq
              ++ withNewChild showTy tab ty
          in
            Seq.iterate op++
              (showSigExp tab sigexp)
              (Seq.map showElem elems)
          end
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
