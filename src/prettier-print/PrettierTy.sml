(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierTy:
sig
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab
  val showTy: tab -> Ast.Ty.t -> doc
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  infix 2 ++
  fun x ++ y = concat (x, y)
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab

  fun showTyNewChild tab ty = newTab tab (fn inner => at inner ++ showTy inner ty)

  and showTy tab ty =
    let
      open Ast.Ty
    in
      case ty of
        Var tok =>
          token tok

      | Con {args = Ast.SyntaxSeq.Empty, id} =>
          token (MaybeLongToken.getToken id)

      | Con {args, id} =>
          showSyntaxSeq tab args (showTy tab)
          ++ token (MaybeLongToken.getToken id)

      | Parens {left, ty, right} =>
          token left ++ nospace ++ showTyNewChild tab ty ++ nospace ++ token right

      | Tuple {elems, delims} =>
          let
            fun f (delim, x) = token delim ++ showTy tab x
          in
            Seq.iterate op++
              (showTy tab (Seq.nth elems 0))
              (Seq.zipWith f (delims, Seq.drop elems 1))
          end

      | Record {left, elems, delims, right} =>
          let
            fun showElem {lab, colon, ty} =
              token lab ++ token colon ++ showTy tab ty
          in
            sequenceAt tab left delims right (Seq.map showElem elems)
          end

      | Arrow {from, arrow, to} =>
          let
            fun loop (arr, right) =
              at tab ++ token arr ++
              (case right of
                Arrow {from, arrow, to} =>
                  showTyNewChild tab from ++ loop (arrow, to)
              | _ =>
                  showTyNewChild tab right)
          in
            showTyNewChild tab from ++ loop (arrow, to)
          end
    end

end
