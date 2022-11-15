(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierExpAndDec:
sig
  type doc = TabbedStringDoc.tab TabbedTokenDoc.t
  type tab = TabbedStringDoc.tab TabbedTokenDoc.tab
  val showExp: Ast.Exp.exp -> doc
  val showExpAt: tab -> Ast.Exp.exp -> doc
  val showDec: Ast.Exp.dec -> doc
  val showDecAt: tab -> Ast.Exp.dec -> doc
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  infix 2 ++
  type doc = TabbedStringDoc.tab TabbedTokenDoc.t
  type tab = TabbedStringDoc.tab TabbedTokenDoc.tab

  fun showTy ty = PrettierTy.showTy ty
  fun showPat pat = PrettierPat.showPat pat

  (* ====================================================================== *)

  fun showExp e = newTab (fn tab => showExpAt tab e)
  and showDec d = newTab (fn tab => showDecAt tab d)

  and showExpAt tab exp =
    let
      open Ast.Exp
    in
      case exp of
        Const tok =>
          token tok
      | Unit {left, right} =>
          token left ++ token right
      | Ident {opp, id} =>
          separateWithSpaces
            [ Option.map token opp
            , SOME (token (MaybeLongToken.getToken id))
            ]
      | Parens {left, exp, right} =>
          token left ++ showExp exp ++ token right
      | Tuple {left, elems, delims, right} =>
          sequence left delims right (Seq.map showExp elems)
      | Sequence {left, elems, delims, right} =>
          sequence left delims right (Seq.map showExp elems)
      | List {left, elems, delims, right} =>
          sequence left delims right (Seq.map showExp elems)
      | Record {left, elems, delims, right} =>
          let
            fun showRow {lab, eq, exp} =
              token lab ++ space ++ token eq ++ showExp exp
          in
            sequence left delims right (Seq.map showRow elems)
          end
      | Select {hash, label} =>
          token hash ++ space ++ token label
      | App {left, right} =>
          showExp left ++ space ++ showExp right

      | _ => text "<exp>"
    end

  and showDecAt tab dec =
    let
      open Ast.Exp
    in
      case dec of
        DecVal {vall, tyvars, elems, delims} =>
          let
            fun mk (delim, {recc, pat, eq, exp}) =
              breakspace tab 
              ++
              separateWithSpaces
                [ SOME (token delim)
                , Option.map token recc
                , SOME (showPat pat)
                , SOME (token eq)
                ]
              ++ space ++ showExp exp

            val first =
              let
                val {recc, pat, eq, exp} = Seq.nth elems 0
              in
                separateWithSpaces
                  [ SOME (token vall)
                  , maybeShowSyntaxSeq tyvars token
                  , Option.map token recc
                  , SOME (showPat pat)
                  , SOME (token eq)
                  ]
                ++ space ++ showExp exp
              end
          in
            Seq.iterate op++ first
            (Seq.map mk (Seq.zip (delims, Seq.drop elems 1)))
          end
(*

*)

      | _ => text "<dec>"
    end

end
