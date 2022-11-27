(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierPat:
sig
  val showPat: Ast.Pat.t PrettierUtil.shower
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  infix 2 ++
  fun x ++ y = concat (x, y)
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab

  fun showTy tab ty = PrettierTy.showTy tab ty

  (* ====================================================================== *)

  fun showPatNewChild tab pat =
    newTab tab (fn inner => at inner ++ showPat inner pat)

  and showPat tab pat =
    let
      open Ast.Pat
    in
      case pat of
        Wild tok =>
          token tok

      | Const tok =>
          token tok

      | Unit {left, right} =>
          token left ++ nospace ++ token right

      | Ident {opp, id} =>
          showOption token opp ++ token (MaybeLongToken.getToken id)

      | Parens {left, pat, right} =>
          token left ++ nospace ++ showPatNewChild tab pat ++ nospace ++ token right

      | Tuple {left, elems, delims, right} =>
          sequenceAt tab left delims right (Seq.map (showPatNewChild tab) elems)

      | List {left, elems, delims, right} =>
          sequenceAt tab left delims right (Seq.map (showPatNewChild tab) elems)

      | Record {left, elems, delims, right} =>
          let
            fun showPatRow patrow =
              case patrow of
                DotDotDot ddd => token ddd
              | LabEqPat {lab, eq, pat} =>
                  token lab ++ token eq ++ showPatNewChild tab pat
              | LabAsPat {id, ty, aspat} =>
                  token id ++
                  showOption (fn {colon, ty} => token colon ++ withNewChild showTy tab ty) ty ++
                  showOption (fn {ass, pat} => token ass ++ showPatNewChild tab pat) aspat
          in
            sequenceAt tab left delims right (Seq.map showPatRow elems)
          end

      | Con {opp, id, atpat} =>
          showOption token opp
          ++ token (MaybeLongToken.getToken id)
          ++ showPatNewChild tab atpat

      | Typed {pat, colon, ty} =>
          showPat tab pat ++ token colon ++ withNewChild showTy tab ty

      | Layered {opp, id, ty, ass, pat} =>
          showOption token opp ++
          token id ++
          showOption (fn {colon, ty} => token colon ++ withNewChild showTy tab ty) ty ++
          token ass ++
          showPatNewChild tab pat

      | Infix {left, id, right} =>
          showPat tab left ++ token id ++ showPatNewChild tab right
    end
end
