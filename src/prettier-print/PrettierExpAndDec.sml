(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierExpAndDec:
sig
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab
  val showExp: tab -> Ast.Exp.exp -> doc
  val showDec: tab -> Ast.Exp.dec -> doc
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


  (* returns SOME (left, token, right) if `<left> <token> <right>` can be
   * viewed as an infix expression.
   *)
  fun tryViewAsInfix exp =
    let
      open Ast.Exp
    in
      case exp of
        Infix {left, id, right} => SOME (left, id, right)
      | Orelse {left, orelsee, right} => SOME (left, orelsee, right)
      | Andalso {left, andalsoo, right} => SOME (left, andalsoo, right)
      | _ => NONE
    end

  (* ====================================================================== *)

  fun showExpNewChild current e = newChildTab current (fn tab => at tab ++ showExp tab e)
  and showDecNewChild current d = newChildTab current (fn tab => at tab ++ showDec tab d)

  and showExp tab exp =
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
          token left ++ nospace ++ showExpNewChild tab exp ++ nospace ++ token right
      | Tuple {left, elems, delims, right} =>
          sequenceAt tab left delims right (Seq.map (showExpNewChild tab) elems)
      | Sequence {left, elems, delims, right} =>
          sequenceAt tab left delims right (Seq.map (showExpNewChild tab) elems)
      | List {left, elems, delims, right} =>
          sequenceAt tab left delims right (Seq.map (showExpNewChild tab) elems)
      | Record {left, elems, delims, right} =>
          let
            fun showRow {lab, eq, exp} =
              token lab ++ token eq ++ (showExpNewChild tab) exp
          in
            sequenceAt tab left delims right (Seq.map showRow elems)
          end
      | Select {hash, label} =>
          token hash ++ nospace ++ token label
      | App {left, right} =>
          showExp tab left ++ showExpNewChild tab right
          (*let
            val (funcExp, args) = appChain [] exp
            fun withBreak tab (a, b) = a ++ breakspace tab ++ b
          in
            showExp tab funcExp ++ space
            ++ newTab (fn inner =>
              Seq.iterate
                (withBreak inner)
                (showExp inner (Seq.nth args 0))
                (Seq.drop (Seq.map (showExp inner) args) 1))
          end*)
      | Typed {exp, colon, ty} =>
          showExpNewChild tab exp ++ token colon ++ showTy ty
      | IfThenElse _ (*{iff, exp1, thenn, exp2, elsee, exp3}*) =>
          showIfThenElseAt tab exp
      | LetInEnd xxx =>
          showLetInEndAt tab xxx
      | Fn {fnn, elems, delims} =>
          newChildTab tab (fn inner => (* do we need the newTab here? *)
          let
            fun mk (delim, {pat, arrow, exp}) =
              at inner
              ++ cond inner {inactive=empty, active=space} ++ token delim
              ++ showPat pat ++ token arrow
              ++ showExpNewChild inner exp
            
            val {pat, arrow, exp} = Seq.nth elems 0
            val initial = at inner ++ token fnn ++ showPat pat ++ token arrow ++ showExpNewChild inner exp
          in
            Seq.iterate op++ initial (Seq.map mk (Seq.zip (delims, Seq.drop elems 1)))
          end)
      | Case {casee, exp=expTop, off, elems, delims} =>
          newChildTab tab (fn inner =>
            let
              fun showBranch {pat, arrow, exp} =
                showPat pat ++ token arrow ++ showExpNewChild inner exp
              fun mk (delim, branch) =
                at inner
                ++ (case delim of
                     SOME d => token d
                   | _ => cond inner {active = space ++ space, inactive = empty})
                ++ showBranch branch
            in
              at inner
              ++ token casee ++ showExpNewChild inner expTop ++ token off
              ++ Seq.iterate op++ (mk (NONE, Seq.nth elems 0))
                   (Seq.zipWith mk (Seq.map SOME delims, Seq.drop elems 1))
            end)
      | Infix {left, id, right} =>
          showInfixedExpAt tab (left, id, right)
      | Andalso {left, andalsoo, right} =>
          showInfixedExpAt tab (left, andalsoo, right)
      | Orelse {left, orelsee, right} =>
          showInfixedExpAt tab (left, orelsee, right)
      | _ => text "<exp>"
    end


  (* TODO: This is still not quite right... *)
  and showInfixedExpAt tab (l, t, r) =
    newChildTab tab (fn inner =>
      let
        open Ast.Exp

        fun tryLeft () =
          case tryViewAsInfix l of
            NONE => NONE
          | SOME (ll, lt, lr) =>
              if not (Token.same (t, lt)) then
                NONE
              else
                SOME
                  (at tab ++ showInfixedExpAt inner (ll, lt, lr)
                  ++ at inner ++ token t ++ showExpNewChild inner r)

        fun tryRight () =
          case tryViewAsInfix r of
            NONE => NONE
          | SOME (rl, rt, rr) =>
              if not (Token.same (t, rt)) then
                NONE
              else
                SOME
                  (showExp tab l ++ at inner ++ token t
                  ++ showInfixedExpAt inner (rl, rt, rr))

        fun normal () =
          at tab ++ showExp inner l ++ at inner ++ token t ++ showExpNewChild inner r
      in
        case tryLeft () of
          SOME x => x
        | NONE =>

        case tryRight () of
          SOME x => x
        | NONE =>

        normal ()
      end)
  

  and showLetInEndAt outerTab {lett, dec, inn, exps, delims, endd} =
    let
      val numExps = Seq.length exps
      fun d i = Seq.nth delims i
      fun withDelims innerTab =
        Seq.mapIdx (fn (i, e) =>
          at innerTab
          ++ showExp innerTab e
          ++ (if i = numExps - 1 then empty else nospace ++ token (d i)))
        exps
    in
      token lett ++ showDecNewChild outerTab dec ++
      at outerTab ++ token inn ++
      newChildTab outerTab (fn innerTab => Seq.iterate op++ empty (withDelims innerTab))
      ++ at outerTab ++ token endd
    end


  and showIfThenElseAt outer exp =
    newChildTab outer (fn inner2 =>
    newChildTab outer (fn inner1 =>
    let
      open Ast.Exp
      val (chain, last) = ifThenElseChain [] exp

      fun breakShowAt tab e = at tab ++ showExp tab e
      
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


  and showDec tab dec =
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
              ++ showExpNewChild tab exp

            val first =
              let
                val {recc, pat, eq, exp} = Seq.nth elems 0
              in
                token vall ++ showSyntaxSeq tab tyvars token
                ++ showOption token recc
                ++ showPat pat
                ++ token eq
                ++ showExpNewChild tab exp
              end
          in
            Seq.iterate op++ first
            (Seq.map mk (Seq.zip (delims, Seq.drop elems 1)))
          end

      | DecFun args =>
          showDecFunAt tab args

      | _ => text "<dec>"
    end


  and showDecFunAt tab {funn, tyvars, fvalbind={elems, delims}} =
    let
      open Ast.Exp

      fun showArgs args =
        Seq.iterate op++ empty (Seq.map showPat args)

      fun showInfixed larg id rarg =
        showPat larg ++ token id ++ showPat rarg

      fun showFNameArgs xx =
        case xx of
          PrefixedFun {opp, id, args} =>
            showOption token opp ++ token id ++ showArgs args
        | InfixedFun {larg, id, rarg} =>
            showInfixed larg id rarg
        | CurriedInfixedFun {lparen, larg, id, rarg, rparen, args} =>
            token lparen ++ nospace
            ++ showInfixed larg id rarg ++ nospace ++ token rparen
            ++ showArgs args

      fun showColonTy {ty: Ast.Ty.t, colon: Token.t} =
        token colon ++ showTy ty

      fun showClause isFirst (front, {fname_args, ty, eq, exp}) =
        at tab
        ++ (if isFirst then empty else cond tab {active=space++space, inactive=empty})
        ++ front
        ++ showFNameArgs fname_args
        ++ showOption showColonTy ty
        ++ token eq
        ++ showExpNewChild tab exp

      fun mkFunction (starter, {elems=innerElems, delims}) =
        let in
          Seq.iterate op++ 
            (showClause true (starter, Seq.nth innerElems 0))
            (Seq.zipWith (showClause false)
              (Seq.map token delims, Seq.drop innerElems 1))
        end

      val front =
        token funn ++ showSyntaxSeq tab tyvars token
    in
      Seq.iterate op++
        (mkFunction (front, Seq.nth elems 0))
        (Seq.zipWith mkFunction (Seq.map token delims, Seq.drop elems 1))
    end

end
