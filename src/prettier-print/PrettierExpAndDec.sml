(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierExpAndDec:
sig
  val showExp: Ast.Exp.exp PrettierUtil.shower
  val showDec: Ast.Exp.dec PrettierUtil.shower
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  open PrettierTy
  open PrettierPat
  infix 2 ++
  fun x ++ y = concat (x, y)

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

  fun decIsEmpty e =
    case e of
      Ast.Exp.DecEmpty => true
    | _ => false

  (* ====================================================================== *)

  fun showTypbind tab (front, typbind: Ast.Exp.typbind as {elems, delims}) =
    let
      fun showOne first (starter, {tyvars, tycon, ty, eq}) =
        (if first then empty else at tab) ++
        token starter ++
        showSyntaxSeq token tab tyvars ++
        token tycon ++
        token eq ++
        withNewChild showTy tab ty
    in
      Seq.iterate op++
        (showOne true (front, Seq.nth elems 0))
        (Seq.zipWith (showOne false) (delims, Seq.drop elems 1))
    end


  fun showDatbind tab (front, datbind: Ast.Exp.datbind as {elems, delims}) =
    let
      fun showCon (starter, {opp, id, arg}) =
        at tab ++ starter ++
        showOption token opp ++
        token id ++
        showOption (fn {off, ty} => token off ++ withNewChild showTy tab ty) arg

      fun showOne first (starter, {tyvars, tycon, eq, elems, delims}) =
        let
          val initial =
            (if first then empty else at tab) ++
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
        (showOne true (front, Seq.nth elems 0))
        (Seq.zipWith (showOne false) (delims, Seq.drop elems 1))
    end

  (* ====================================================================== *)

  fun showExp tab exp =
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
          token left ++ nospace ++ withNewChild showExp tab exp ++ nospace ++ token right

      | Tuple {left, elems, delims, right} =>
          showSequence tab
            { openn = left
            , elems = Seq.map (withNewChild showExp tab) elems
            , delims = delims
            , close = right
            }

      | Sequence {left, elems, delims, right} =>
          showSequence tab
            { openn = left
            , elems = Seq.map (withNewChild showExp tab) elems
            , delims = delims
            , close = right
            }

      | List {left, elems, delims, right} =>
          showSequence tab
            { openn = left
            , elems = Seq.map (withNewChild showExp tab) elems
            , delims = delims
            , close = right
            }

      | Record {left, elems, delims, right} =>
          let
            fun showRow {lab, eq, exp} =
              token lab ++ token eq ++ (withNewChild showExp tab) exp
          in
            showSequence tab
              { openn = left
              , elems = Seq.map showRow elems
              , delims = delims
              , close = right
              }
          end

      | Select {hash, label} =>
          token hash ++ nospace ++ token label

      | App {left, right} =>
          showExp tab left ++ withNewChild showExp tab right
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
          withNewChild showExp tab exp ++ token colon ++ withNewChild showTy tab ty

      | IfThenElse _ (*{iff, exp1, thenn, exp2, elsee, exp3}*) =>
          showIfThenElseAt tab exp

      | LetInEnd xxx =>
          showLetInEndAt tab xxx

      | Fn {fnn, elems, delims} =>
          newTab tab (fn inner => (* do we need the newTab here? *)
          let
            fun mk (delim, {pat, arrow, exp}) =
              at inner
              ++ cond inner {inactive=empty, active=space} ++ token delim
              ++ withNewChild showPat inner pat ++ token arrow
              ++ withNewChild showExp inner exp

            val {pat, arrow, exp} = Seq.nth elems 0
            val initial = at inner ++ token fnn ++ withNewChild showPat inner pat ++ token arrow ++ withNewChild showExp inner exp
          in
            Seq.iterate op++ initial (Seq.map mk (Seq.zip (delims, Seq.drop elems 1)))
          end)

      | Case {casee, exp=expTop, off, elems, delims} =>
          newTab tab (fn inner =>
            let
              fun showBranch {pat, arrow, exp} =
                withNewChild showPat inner pat ++ token arrow ++ withNewChild showExp inner exp
              fun mk (delim, branch) =
                at inner
                ++ (case delim of
                     SOME d => token d
                   | _ => cond inner {active = space ++ space, inactive = empty})
                ++ showBranch branch
            in
              at inner
              ++ token casee ++ withNewChild showExp inner expTop ++ token off
              ++ Seq.iterate op++ (mk (NONE, Seq.nth elems 0))
                   (Seq.zipWith mk (Seq.map SOME delims, Seq.drop elems 1))
            end)

      | Infix {left, id, right} =>
          showInfixedExpAt tab (left, id, right)

      | Andalso {left, andalsoo, right} =>
          showInfixedExpAt tab (left, andalsoo, right)

      | Orelse {left, orelsee, right} =>
          showInfixedExpAt tab (left, orelsee, right)

      | While {whilee, exp1, doo, exp2} =>
          newTab tab (fn inner1 =>
          newTab tab (fn inner2 =>
            token whilee ++
            at inner1 ++ showExp inner1 exp1 ++
            cond inner1 {inactive = empty, active = at tab} ++
            token doo ++
            at inner2 ++ showExp inner2 exp2))

      | Raise {raisee, exp} =>
          token raisee ++ withNewChild showExp tab exp

      | Handle {exp=expLeft, handlee, elems, delims} =>
          newTab tab (fn inner =>
            let
              fun showBranch {pat, arrow, exp} =
                withNewChild showPat inner pat ++ token arrow ++ withNewChild showExp inner exp
              fun mk (delim, branch) =
                at inner
                ++ (case delim of
                     SOME d => token d
                   | _ => cond inner {active = space ++ space, inactive = empty})
                ++ showBranch branch
            in
              showExp tab expLeft ++
              at inner ++
              token handlee ++
              Seq.iterate op++ (mk (NONE, Seq.nth elems 0))
                (Seq.zipWith mk (Seq.map SOME delims, Seq.drop elems 1))
            end)

      | MLtonSpecific {underscore, directive, contents, semicolon} =>
          token underscore ++ nospace ++ token directive
          ++ Seq.iterate op++ empty (Seq.map token contents)
          ++ nospace ++ token semicolon

    end


  (* TODO: This is still not quite right... *)
  and showInfixedExpAt tab (l, t, r) =
    newTab tab (fn inner =>
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
                  ++ at inner ++ token t ++ withNewChild showExp inner r)

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
          at tab ++ showExp inner l ++ at inner ++ token t ++ withNewChild showExp inner r
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
      showThingSimilarToLetInEnd outerTab
        { lett = lett
        , isEmpty1 = decIsEmpty dec
        , doc1 = withNewChildWithStyle Indented showDec outerTab dec
        , inn = inn
        , doc2 =
            newTabWithStyle outerTab (Indented, fn innerTab =>
              Seq.iterate op++ empty (withDelims innerTab))
        , endd = endd
        }
    end


  and showIfThenElseAt outer exp =
    newTabWithStyle outer (Indented, fn inner2 =>
    newTabWithStyle outer (Indented, fn inner1 =>
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
        DecEmpty => empty

      | DecVal {vall, tyvars, elems, delims} =>
          let
            fun mk (delim, {recc, pat, eq, exp}) =
              at tab ++ token delim
              ++ showOption token recc
              ++ withNewChild showPat tab pat
              ++ token eq
              ++ withNewChild showExp tab exp

            val first =
              let
                val {recc, pat, eq, exp} = Seq.nth elems 0
              in
                token vall ++ showSyntaxSeq token tab tyvars
                ++ showOption token recc
                ++ withNewChild showPat tab pat
                ++ token eq
                ++ withNewChild showExp tab exp
              end
          in
            Seq.iterate op++ first
            (Seq.map mk (Seq.zip (delims, Seq.drop elems 1)))
          end

      | DecFun args =>
          showDecFunAt tab args

      | DecInfix {infixx, precedence, elems} =>
          token infixx ++ showOption token precedence
          ++ Seq.iterate op++ empty (Seq.map token elems)

      | DecInfixr {infixrr, precedence, elems} =>
          token infixrr ++ showOption token precedence
          ++ Seq.iterate op++ empty (Seq.map token elems)

      | DecNonfix {nonfixx, elems} =>
          token nonfixx ++ Seq.iterate op++ empty (Seq.map token elems)

      | DecMultiple {elems, delims} =>
          let
            fun mk first (elem, delim) =
              (if first then empty else at tab)
              ++ showDec tab elem
              ++ showOption (fn d => nospace ++ token d) delim

            val things = Seq.zip (elems, delims)
          in
            Seq.iterate op++
              (mk true (Seq.nth things 0))
              (Seq.map (mk false) (Seq.drop things 1))
          end

      | DecOpen {openn, elems} =>
          token openn ++
          Seq.iterate op++ empty
            (Seq.map (token o MaybeLongToken.getToken) elems)

      | DecType {typee, typbind} =>
          showTypbind tab (typee, typbind)

      | DecDatatype {datatypee, datbind, withtypee} =>
          showDatbind tab (datatypee, datbind)
          ++ showOption (fn {withtypee, typbind} =>
               at tab ++ showTypbind tab (withtypee, typbind))
             withtypee

      | DecException {exceptionn, elems, delims} =>
          let
            fun showExbind exbind =
              case exbind of
                ExnNew {opp, id, arg} =>
                  showOption token opp
                  ++ token id
                  ++ showOption (fn {off, ty} => token off ++ withNewChild showTy tab ty) arg
              | ExnReplicate {opp, left_id, eq, right_id} =>
                  showOption token opp
                  ++ token left_id
                  ++ token eq
                  ++ token (MaybeLongToken.getToken right_id)

            fun showOne first (starter, elem) =
              (if first then empty else at tab)
              ++ token starter ++ showExbind elem
          in
            Seq.iterate op++
              (showOne true (exceptionn, Seq.nth elems 0))
              (Seq.zipWith (showOne false) (delims, Seq.drop elems 1))
          end

      | DecReplicateDatatype
          {left_datatypee, left_id, eq, right_datatypee, right_id} =>
          Seq.iterate op++ empty
            (Seq.map token (Seq.fromList
              [ left_datatypee
              , left_id
              , eq
              , right_datatypee
              , MaybeLongToken.getToken right_id
              ]))

      | DecLocal {locall, left_dec, inn, right_dec, endd} =>
          showThingSimilarToLetInEnd tab
            { lett = locall
            , isEmpty1 = decIsEmpty left_dec
            , doc1 = withNewChildWithStyle Indented showDec tab left_dec
            , inn = inn
            , doc2 = withNewChildWithStyle Indented showDec tab right_dec
            , endd = endd
            }

      | DecAbstype {abstypee, datbind, withtypee, withh, dec, endd} =>
          let
            val datbinds =
              showDatbind tab (abstypee, datbind)

            val bottom =
              at tab
              ++ token withh
              ++ withNewChildWithStyle Indented showDec tab dec
              ++ at tab
              ++ token endd
          in
            showDatbind tab (abstypee, datbind)
            ++ showOption (fn {withtypee, typbind} =>
                 at tab ++ showTypbind tab (withtypee, typbind))
               withtypee
            ++ bottom
          end
    end


  and showDecFunAt tab {funn, tyvars, fvalbind={elems, delims}} =
    let
      open Ast.Exp

      fun showArgs args =
        Seq.iterate op++ empty (Seq.map (withNewChild showPat tab) args)

      fun showInfixed larg id rarg =
        withNewChild showPat tab larg ++ token id ++ withNewChild showPat tab rarg

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
        token colon ++ withNewChild showTy tab ty

      fun showClause isVeryTop isFirst (front, {fname_args, ty, eq, exp}) =
        (if isVeryTop then empty else at tab)
        ++ (if isFirst then empty else cond tab {active=space++space, inactive=empty})
        ++ front
        ++ showFNameArgs fname_args
        ++ showOption showColonTy ty
        ++ token eq
        ++ withNewChild showExp tab exp

      fun mkFunction isVeryTop (starter, {elems=innerElems, delims}) =
        let in
          Seq.iterate op++
            (showClause isVeryTop true (starter, Seq.nth innerElems 0))
            (Seq.zipWith (showClause isVeryTop false)
              (Seq.map token delims, Seq.drop innerElems 1))
        end

      val front =
        token funn ++ showSyntaxSeq token tab tyvars
    in
      Seq.iterate op++
        (mkFunction true (front, Seq.nth elems 0))
        (Seq.zipWith (mkFunction false) (Seq.map token delims, Seq.drop elems 1))
    end

end
