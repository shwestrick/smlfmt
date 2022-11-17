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

  fun ifThenElseChain acc exp =
    case exp of
      Ast.Exp.IfThenElse {iff, exp1, thenn, exp2, elsee, exp3} =>
        ifThenElseChain ({iff=iff, exp1=exp1, thenn=thenn, exp2=exp2, elsee=elsee} :: acc) exp3
    | _ => (Seq.fromRevList acc, exp)


  (* returns (function, curried args) *)
  fun appChain acc exp =
    case exp of
      Ast.Exp.App {left, right} => appChain (right :: acc) left
    | _ => (exp, Seq.fromList acc)

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
          sequenceAt tab left delims right (Seq.map showExp elems)
      | Sequence {left, elems, delims, right} =>
          sequenceAt tab left delims right (Seq.map showExp elems)
      | List {left, elems, delims, right} =>
          sequenceAt tab left delims right (Seq.map showExp elems)
      | Record {left, elems, delims, right} =>
          let
            fun showRow {lab, eq, exp} =
              token lab ++ space ++ token eq ++ showExp exp
          in
            sequenceAt tab left delims right (Seq.map showRow elems)
          end
      | Select {hash, label} =>
          token hash ++ space ++ token label
      | App {left, right} =>
          showExpAt tab left ++ space ++ showExp right
          (*let
            val (funcExp, args) = appChain [] exp
            fun withBreak tab (a, b) = a ++ breakspace tab ++ b
          in
            showExpAt tab funcExp ++ space
            ++ newTab (fn inner =>
              Seq.iterate
                (withBreak inner)
                (showExpAt inner (Seq.nth args 0))
                (Seq.drop (Seq.map (showExpAt inner) args) 1))
          end*)
      | Typed {exp, colon, ty} =>
          showExp exp ++ space ++ token colon ++ space ++ showTy ty
      | IfThenElse _ (*{iff, exp1, thenn, exp2, elsee, exp3}*) =>
          showIfThenElseAt tab exp
      | LetInEnd xxx =>
          showLetInEndAt tab xxx
      | _ => text "<exp>"
    end

  
  (* Not quite right. Using the same tab for both the top and bottom
   * can do weird things... *)
  and showLetInEndAt outerTab {lett, dec, inn, exps, delims, endd} =
    let
      val numExps = Seq.length exps
      fun d i = Seq.nth delims i
      fun withDelims innerTab =
        Seq.mapIdx (fn (i, e) =>
          breakspace innerTab
          ++ showExpAt innerTab e
          ++ (if i = numExps - 1 then empty else token (d i)))
        exps
    in
      token lett ++ space ++
      newTab (fn innerTab =>
        showDecAt innerTab dec
        ++ breakspace outerTab ++ token inn
        ++ Seq.iterate op++ empty (withDelims innerTab))
      ++ breakspace outerTab ++ token endd
    end


  and showIfThenElseAt outerTab exp =
    let
      open Ast.Exp
      val (chain, last) = ifThenElseChain [] exp

      fun f innerTab i =
        let
          val {iff, exp1, thenn, exp2, elsee} = Seq.nth chain i
        in
          space ++ token iff
          ++ space ++ showExp exp1
          ++ space ++ token thenn
          ++ breakspace innerTab ++ showExpAt innerTab exp2
          ++ breakspace outerTab ++ token elsee
        end

      val {iff, exp1, thenn, exp2, elsee} = Seq.nth chain 0
    in
      token iff ++ space ++ showExp exp1 ++ space ++ token thenn ++ space
      ++ newTab (fn innerTab =>
        showExpAt innerTab exp2
        ++ breakspace outerTab ++ token elsee
        ++ Util.loop (1, Seq.length chain) empty (fn (d, i) => d ++ f innerTab i)
        ++ breakspace innerTab
        ++ showExpAt innerTab last)
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
