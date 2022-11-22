(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierExpAndDec:
sig
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab
  val showExp: tab -> Ast.Exp.exp -> doc
  val showExpAt: tab -> Ast.Exp.exp -> doc
  val showDec: tab -> Ast.Exp.dec -> doc
  val showDecAt: tab -> Ast.Exp.dec -> doc
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  infix 2 ++
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab

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

  fun showExp current e = newChildTab current (fn tab => at tab ++ showExpAt tab e)
  and showDec current d = newChildTab current (fn tab => at tab ++ showDecAt tab d)

  and showExpAt tab exp =
    let
      open Ast.Exp
    in
      case exp of
        Const tok =>
          token tok
      | Unit {left, right} =>
          token left ++ nospace ++ token right
      | Ident {opp, id} =>
          showOption token opp ++ token (MaybeLongToken.getToken id)
      | Parens {left, exp, right} =>
          token left ++ nospace ++ showExp tab exp ++ nospace ++ token right
      | Tuple {left, elems, delims, right} =>
          sequenceAt tab left delims right (Seq.map (showExp tab) elems)
      | Sequence {left, elems, delims, right} =>
          sequenceAt tab left delims right (Seq.map (showExp tab) elems)
      | List {left, elems, delims, right} =>
          sequenceAt tab left delims right (Seq.map (showExp tab) elems)
      | Record {left, elems, delims, right} =>
          let
            fun showRow {lab, eq, exp} =
              token lab ++ token eq ++ (showExp tab) exp
          in
            sequenceAt tab left delims right (Seq.map showRow elems)
          end
      | Select {hash, label} =>
          token hash ++ nospace ++ token label
      | App {left, right} =>
          showExpAt tab left ++ showExp tab right
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
          showExp tab exp ++ token colon ++ showTy ty
      | IfThenElse _ (*{iff, exp1, thenn, exp2, elsee, exp3}*) =>
          showIfThenElseAt tab exp
      | LetInEnd xxx =>
          showLetInEndAt tab xxx
      | Fn {fnn, elems, delims} =>
          newTab (fn inner => (* do we need the newTab here? *)
          let
            fun mk (delim, {pat, arrow, exp}) =
              at inner
              ++ cond inner {inactive=empty, active=space} ++ token delim
              ++ showPat pat ++ token arrow
              ++ showExp inner exp
            
            val {pat, arrow, exp} = Seq.nth elems 0
            val initial = at inner ++ token fnn ++ showPat pat ++ token arrow ++ showExp inner exp
          in
            Seq.iterate op++ initial (Seq.map mk (Seq.zip (delims, Seq.drop elems 1)))
          end)
      | Case {casee, exp=expTop, off, elems, delims} =>
          newTab (fn inner =>
            let
              fun showBranch {pat, arrow, exp} =
                showPat pat ++ token arrow ++ showExp inner exp
              fun mk (delim, branch) =
                at inner
                ++ (case delim of
                     SOME d => token d
                   | _ => cond inner {active = space ++ space, inactive = empty})
                ++ showBranch branch
            in
              at inner
              ++ token casee ++ showExp inner expTop ++ token off
              ++ Seq.iterate op++ (mk (NONE, Seq.nth elems 0))
                   (Seq.zipWith mk (Seq.map SOME delims, Seq.drop elems 1))
            end)
      | _ => text "<exp>"
    end

  
  and showLetInEndAt outerTab {lett, dec, inn, exps, delims, endd} =
    let
      val numExps = Seq.length exps
      fun d i = Seq.nth delims i
      fun withDelims innerTab =
        Seq.mapIdx (fn (i, e) =>
          at innerTab
          ++ showExpAt innerTab e
          ++ (if i = numExps - 1 then empty else token (d i)))
        exps
    in
      token lett ++ showDec outerTab dec ++
      at outerTab ++ token inn ++
      newTab (fn innerTab => Seq.iterate op++ empty (withDelims innerTab))
      ++ at outerTab ++ token endd
    end


  and showIfThenElseAt outer exp =
    newChildTab outer (fn inner2 =>
    newChildTab outer (fn inner1 =>
    let
      open Ast.Exp
      val (chain, last) = ifThenElseChain [] exp

      fun breakShowAt tab e = at tab ++ showExpAt tab e
      
      fun f i =
        let
          val {iff, exp1, thenn, exp2, elsee} = Seq.nth chain i
        in
          token iff ++
          breakShowAt inner1 exp1 ++
          cond inner1 {inactive = empty, active = at outer} ++
          token thenn ++
          breakShowAt inner2 exp2 ++
          at outer ++ token elsee
        end
    in
      Util.loop (0, Seq.length chain) empty (fn (d, i) => d ++ f i)
      ++
      breakShowAt inner2 last
    end))


  and showDecAt tab dec =
    let
      open Ast.Exp
    in
      case dec of
        DecVal {vall, tyvars, elems, delims} =>
          let
            fun mk (delim, {recc, pat, eq, exp}) =
              at tab ++ token delim
              ++ showOption token recc
              ++ showPat pat
              ++ token eq
              ++ showExp tab exp

            val first =
              let
                val {recc, pat, eq, exp} = Seq.nth elems 0
              in
                token vall ++ showSyntaxSeq tab tyvars token
                ++ showOption token recc
                ++ showPat pat
                ++ token eq
                ++ showExp tab exp
              end
          in
            Seq.iterate op++ first
            (Seq.map mk (Seq.zip (delims, Seq.drop elems 1)))
          end

      | _ => text "<dec>"
    end

end
