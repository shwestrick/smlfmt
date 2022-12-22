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


  fun tryViewExpAsSimpleIdentifier e =
    case e of
      Ast.Exp.Ident {opp=NONE, id} =>
        if MaybeLongToken.isLong id then
          NONE
        else
          SOME (MaybeLongToken.getToken id)

    | _ => NONE


  fun appWantsToTouch left right =
    let
      open Ast.Exp

      val rightNice =
        case right of
          Ident {opp=NONE, id} =>
            not (MaybeLongToken.isLong id)
            andalso
            not (Token.isSymbolicIdentifier (MaybeLongToken.getToken id))
        | Tuple _ => true
        | List _ => true
        | Sequence _ => true
        | Parens _ => true
        | Record _ => true
        | Unit _ => true
        | _ => false
    in
      rightNice
      andalso
      case tryViewExpAsSimpleIdentifier left of
        SOME id =>
          Token.isSymbolicIdentifier id
          andalso not (Token.hasCommentsAfter id)
      | NONE => false
    end


  fun appWantsSpace left right =
    not (appWantsToTouch left right)


  (* ====================================================================== *)

  fun showTypbind tab (front, typbind: Ast.Exp.typbind as {elems, delims}) =
    let
      fun showOne first (starter, {tyvars, tycon, ty, eq}) =
        maybeAt tab (not first)
          (token starter ++
          showTokenSyntaxSeq tab tyvars ++
          token tycon ++
          token eq ++
          withNewChild showTy tab ty)
    in
      Seq.iterate op++
        (showOne true (front, Seq.nth elems 0))
        (Seq.zipWith (showOne false) (delims, Seq.drop elems 1))
    end


  fun showDatbind tab (front, datbind: Ast.Exp.datbind as {elems, delims}) =
    newTab tab (fn tab =>
    let
      fun showCon (starter, {opp, id, arg}) =
        at tab
          (starter ++
          showMaybeOpToken opp id ++
          showOption
            (fn {off, ty} =>
              token off ++ withNewChildWithStyle indentedAtLeast4 showTy tab ty)
            arg)

      fun showOne (starter, {tyvars, tycon, eq, elems, delims}) =
        let
          val initial =
            at tab
              (token starter ++
              showTokenSyntaxSeq tab tyvars ++
              token tycon ++
              token eq)

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
        (showOne (front, Seq.nth elems 0))
        (Seq.zipWith showOne (delims, Seq.drop elems 1))
    end)

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
          showMaybeOpToken opp (MaybeLongToken.getToken id)

      | Parens {left, exp, right} =>
          token left ++
          (if expStartsWithStar exp then empty else nospace)
          ++ withNewChild showExp tab exp ++ nospace ++ token right

      | Tuple {left, elems, delims, right} =>
          showSequence expStartsWithStar (withNewChild showExp) tab
            { openn = left
            , elems = elems
            , delims = delims
            , close = right
            }

      | Sequence {left, elems, delims, right} =>
          showSequence expStartsWithStar (withNewChild showExp) tab
            { openn = left
            , elems = elems
            , delims = delims
            , close = right
            }

      | List {left, elems, delims, right} =>
          showSequence expStartsWithStar (withNewChild showExp) tab
            { openn = left
            , elems = elems
            , delims = delims
            , close = right
            }

      | Record {left, elems, delims, right} =>
          let
            fun showRow tab {lab, eq, exp} =
              token lab ++ token eq ++ (withNewChild showExp tab) exp
          in
            showSequence (fn _ => false) (withNewChild showRow) tab
              { openn = left
              , elems = elems
              , delims = delims
              , close = right
              }
          end

      | Select {hash, label} =>
          token hash ++
          (if
            Token.isSymbolicIdentifier label
            orelse Token.hasCommentsAfter hash
          then
            empty
          else
            nospace)
          ++ token label

      | App {left, right} =>
          showExp tab left ++
          (if appWantsSpace left right then empty else nospace)
          ++ withNewChild showExp tab right
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
                (cond inner {inactive=empty, active=space} ++ token delim
                ++ withNewChild showPat inner pat ++ token arrow
                ++ withNewChild showExp inner exp)

            val {pat, arrow, exp} = Seq.nth elems 0
            val initial =
              at inner
                (token fnn
                ++ withNewChild showPat inner pat
                ++ token arrow
                ++ withNewChild showExp inner exp)
          in
            Seq.iterate op++ initial (Seq.map mk (Seq.zip (delims, Seq.drop elems 1)))
          end)

      | Case {casee, exp=expTop, off, elems, delims} =>
          newTab tab (fn inner1 =>
          newTab inner1 (fn inner2 =>
            let
              fun showBranch {pat, arrow, exp} =
                withNewChild showPat inner1 pat
                ++ token arrow
                ++ withNewChildWithStyle indentedAtLeast4 showExp inner1 exp
              fun mk (delim, branch) =
                at inner1
                  ((case delim of
                      SOME d => token d
                    | _ => cond inner1 {active = space ++ space, inactive = empty})
                  ++ showBranch branch)
            in
              at inner1 (token casee)
              ++
              at inner2 (showExp inner2 expTop)
              ++
              cond inner2 {inactive = token off, active = at inner1 (token off)}
              ++
              Seq.iterate op++ (mk (NONE, Seq.nth elems 0))
                (Seq.zipWith mk (Seq.map SOME delims, Seq.drop elems 1))
            end))

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
            at inner1 (showExp inner1 exp1) ++
            cond inner1 {inactive = token doo, active = at tab (token doo)} ++
            at inner2 (showExp inner2 exp2)))

      | Raise {raisee, exp} =>
          token raisee ++ withNewChild showExp tab exp

      | Handle {exp=expLeft, handlee, elems, delims} =>
          newTab tab (fn inner =>
            let
              fun showBranch {pat, arrow, exp} =
                withNewChild showPat inner pat
                ++ token arrow
                ++ withNewChildWithStyle indentedAtLeast4 showExp inner exp
              fun mk (delim, branch) =
                at inner
                  ((case delim of
                      SOME d => token d
                    | _ => cond inner {active = space ++ space, inactive = empty})
                  ++ showBranch branch)
            in
              at tab (showExp tab expLeft)
              ++
              at tab (at inner (token handlee))
              ++
              Seq.iterate op++ (mk (NONE, Seq.nth elems 0))
                (Seq.zipWith mk (Seq.map SOME delims, Seq.drop elems 1))
            end)

      | MLtonSpecific {underscore, directive, contents, semicolon} =>
          token underscore ++ nospace ++ token directive
          ++ Seq.iterate op++ empty (Seq.map token contents)
          ++ nospace ++ token semicolon

    end


  and showInfixedExpAt tab (l, t, r) =
    newTab tab (fn tab =>
    let
      open Ast.Exp

      fun infixChainLeft () =
        let
          fun loop acc exp =
            case tryViewAsInfix exp of
              NONE => (exp, acc)
            | SOME (left, id, right) =>
                if Token.same (t, id) then
                  loop ({id=id, right=right} :: acc) left
                else
                  (exp, acc)
          val (exp, elems) = loop [{id=t, right=r}] l
        in
          (exp, Seq.fromList elems)
        end

      fun infixChainRight () =
        let
          fun loop acc (prevId, exp) =
            case tryViewAsInfix exp of
              NONE => {id=prevId, right=exp} :: acc
            | SOME (left, id, right) =>
                if not (Token.same (t, id)) then
                  {id=prevId, right=exp} :: acc
                else
                  loop ({id=prevId, right=left} :: acc) (id, right)

          val elems = loop [] (t, r)
        in
          (l, Seq.fromRevList elems)
        end

      val (leftmostExp1, rightElems1) = infixChainLeft ()
      val (leftmostExp2, rightElems2) = infixChainRight ()

      fun showRightElem {id, right} =
        at tab (token id) ++ withNewChild showExp tab right

      val (leftmostExp, rightElems) =
        if Seq.length rightElems1 >= Seq.length rightElems2 then
          (leftmostExp1, rightElems1)
        else
          (leftmostExp2, rightElems2)
    in
      at tab (withNewChild showExp tab leftmostExp)
      ++
      Seq.iterate op++ empty (Seq.map showRightElem rightElems)
    end)


  and showLetInEndAt outerTab {lett, dec, inn, exps, delims, endd} =
    let
      val numExps = Seq.length exps
      fun d i = Seq.nth delims i
      fun withDelims innerTab =
        Seq.mapIdx (fn (i, e) =>
          at innerTab
            (showExp innerTab e
            ++ (if i = numExps - 1 then empty else nospace ++ token (d i))))
        exps
    in
      newTabWithStyle outerTab (indented, fn inner =>
        showThingSimilarToLetInEnd outerTab
          { lett = lett
          , isEmpty1 = decIsEmpty dec
          , doc1 = at inner (showDec inner dec)
          , inn = inn
          , doc2 = Seq.iterate op++ empty (withDelims inner)
          , endd = endd
          })
    end


  and showIfThenElseAt outer exp =
    newTabWithStyle outer (indented, fn inner2 =>
    newTabWithStyle outer (indented, fn inner1 =>
    let
      open Ast.Exp
      val (chain, last) = ifThenElseChain [] exp

      fun breakShowAt tab e = at tab (showExp tab e)

      fun f i =
        let
          val {iff, exp1, thenn, exp2, elsee} = Seq.nth chain i
        in
          token iff ++
          breakShowAt inner1 exp1 ++
          cond inner1 {inactive = token thenn, active = at outer (token thenn)} ++
          breakShowAt inner2 exp2 ++
          at outer (token elsee)
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
              at tab
                (token delim
                ++ showOption token recc
                ++ withNewChild showPat tab pat
                ++ token eq
                ++ withNewChild showExp tab exp)

            val first =
              let
                val {recc, pat, eq, exp} = Seq.nth elems 0
              in
                token vall ++ showTokenSyntaxSeq tab tyvars
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
              maybeAt tab (not first)
                (showDec tab elem
                ++ showOption (fn d => nospace ++ token d) delim)

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
               at tab (showTypbind tab (withtypee, typbind)))
             withtypee

      | DecException {exceptionn, elems, delims} =>
          let
            fun showExbind exbind =
              case exbind of
                ExnNew {opp, id, arg} =>
                  showMaybeOpToken opp id
                  ++ showOption (fn {off, ty} => token off ++ withNewChild showTy tab ty) arg
              | ExnReplicate {opp, left_id, eq, right_id} =>
                  showOption token opp
                  ++ token left_id
                  ++ token eq
                  ++ token (MaybeLongToken.getToken right_id)

            fun showOne first (starter, elem) =
              maybeAt tab (not first)
                (token starter ++ showExbind elem)
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
            , doc1 = withNewChildWithStyle (indented) showDec tab left_dec
            , inn = inn
            , doc2 = withNewChildWithStyle (indented) showDec tab right_dec
            , endd = endd
            }

      | DecAbstype {abstypee, datbind, withtypee, withh, dec, endd} =>
          let
            val datbinds =
              showDatbind tab (abstypee, datbind)

            val bottom =
              at tab
                (token withh
                ++ withNewChildWithStyle (indented) showDec tab dec)
              ++
              at tab (token endd)
          in
            showDatbind tab (abstypee, datbind)
            ++ showOption (fn {withtypee, typbind} =>
                 at tab (showTypbind tab (withtypee, typbind)))
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
            showMaybeOpToken opp id ++ showArgs args
        | InfixedFun {larg, id, rarg} =>
            showInfixed larg id rarg
        | CurriedInfixedFun {lparen, larg, id, rarg, rparen, args} =>
            token lparen ++
            (if patStartsWithStar larg then empty else nospace)
            ++ showInfixed larg id rarg ++ nospace ++ token rparen
            ++ showArgs args

      fun showColonTy {ty: Ast.Ty.t, colon: Token.t} =
        token colon ++ withNewChild showTy tab ty

      fun showClause isVeryTop isFirst (front, {fname_args, ty, eq, exp}) =
        maybeAt tab (not isVeryTop)
          ( (if isFirst then empty else cond tab {active=space++space, inactive=empty})
          ++ front
          ++ showFNameArgs fname_args
          ++ showOption showColonTy ty
          ++ token eq
          ++ withNewChild showExp tab exp)

      fun mkFunction isVeryTop (starter, {elems=innerElems, delims}) =
        let in
          Seq.iterate op++
            (showClause isVeryTop true (starter, Seq.nth innerElems 0))
            (Seq.zipWith (showClause isVeryTop false)
              (Seq.map token delims, Seq.drop innerElems 1))
        end

      val front =
        token funn ++ showTokenSyntaxSeq tab tyvars
    in
      Seq.iterate op++
        (mkFunction true (front, Seq.nth elems 0))
        (Seq.zipWith (mkFunction false) (Seq.map token delims, Seq.drop elems 1))
    end

end
