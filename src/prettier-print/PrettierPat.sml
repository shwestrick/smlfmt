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
  open PrettierTy
  infix 2 ++
  fun x ++ y = concat (x, y)

  (* ====================================================================== *)

  fun showPat tab pat =
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
          token left ++ nospace ++ withNewChild showPat tab pat ++ nospace ++ token right

      | Tuple {left, elems, delims, right} =>
          showSequence tab
            { openn = left
            , elems = Seq.map (withNewChild showPat tab) elems
            , delims = delims
            , close = right
            }

      | List {left, elems, delims, right} =>
          showSequence tab
            { openn = left
            , elems = Seq.map (withNewChild showPat tab) elems
            , delims = delims
            , close = right
            }

      | Record {left, elems, delims, right} =>
          let
            fun showPatRow patrow =
              case patrow of
                DotDotDot ddd => token ddd
              | LabEqPat {lab, eq, pat} =>
                  token lab ++ token eq ++ withNewChild showPat tab pat
              | LabAsPat {id, ty, aspat} =>
                  token id ++
                  showOption (fn {colon, ty} => token colon ++ withNewChild showTy tab ty) ty ++
                  showOption (fn {ass, pat} => token ass ++ withNewChild showPat tab pat) aspat
          in
            showSequence tab
              { openn = left
              , elems = Seq.map showPatRow elems
              , delims = delims
              , close = right
              }
          end

      | Con {opp, id, atpat} =>
          showOption token opp
          ++ token (MaybeLongToken.getToken id)
          ++ withNewChild showPat tab atpat

      | Typed {pat, colon, ty} =>
          showPat tab pat ++ token colon ++ withNewChild showTy tab ty

      | Layered {opp, id, ty, ass, pat} =>
          showOption token opp ++
          token id ++
          showOption (fn {colon, ty} => token colon ++ withNewChild showTy tab ty) ty ++
          token ass ++
          withNewChild showPat tab pat

      | Infix {left, id, right} =>
          showPat tab left ++ token id ++ withNewChild showPat tab right
    end
end
