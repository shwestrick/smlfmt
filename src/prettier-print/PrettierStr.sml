(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierStr:
sig
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab
  val showStrExp: tab -> Ast.Str.strexp -> doc
  val showStrDec: tab -> Ast.Str.strdec -> doc
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  infix 2 ++
  type doc = TabbedTokenDoc.t
  type tab = TabbedTokenDoc.tab

  fun showTy tab ty = PrettierTy.showTy tab ty
  fun showDec tab dec = PrettierExpAndDec.showDec tab dec
  fun showSigExp tab sigexp = PrettierSig.showSigExp tab sigexp

  fun showSigExpNewChild tab e =
    newTab tab (fn inner => at inner ++ showSigExp inner e)

  fun showTyNewChild tab ty =
    newTab tab (fn inner => at inner ++ showTy inner ty)

  (* ====================================================================== *)

  fun leftMostStrExp strexp =
    let
      open Ast.Str
    in
      case strexp of
        Constraint {strexp, ...} => leftMostStrExp strexp
      | _ => strexp
    end

  fun sigExpWantsSameTabAsDec e =
    let
      open Ast.Sig
    in
      case e of
        Ident _ => false
      | _ => true
    end

  fun strExpWantsSameTabAsDec e =
    let
      open Ast.Str
    in
      case leftMostStrExp e of
        Struct _ => true
      | LetInEnd _ => true
      | _ => false
    end

  fun strExpInsideFunAppWantsSpaceBefore e =
    let
      open Ast.Str
    in
      case leftMostStrExp e of
        Struct _ => true
      | LetInEnd _ => true
      | _ => false
    end

  fun strDecInsideFunAppWantsSpaceBefore e =
    let
      open Ast.Str
    in
      case e of
        DecEmpty => false
      | DecCore _ => false
      | _ => true
    end

  fun decIsEmpty e =
    case e of
      Ast.Str.DecEmpty => true
    | _ => false

  (* ====================================================================== *)

  fun showStrExpNewChild tab e = newTab tab (fn inner => at inner ++ showStrExp inner e)
  and showStrDecNewChild tab e = newTab tab (fn inner => at inner ++ showStrDec inner e)
  and showStrDecNewChildWithStyle tab style e =
    newTabWithStyle tab (style, fn inner => at inner ++ showStrDec inner e)
  and showStrExpNewChildWithStyle tab style e =
    newTabWithStyle tab (style, fn inner => at inner ++ showStrExp inner e)

  and showStrExp tab e =
    let
      open Ast.Str
    in
      case e of
        Ident id =>
          token (MaybeLongToken.getToken id)
          (* newTab tab (fn tab => at tab ++ ) *)

      | Struct {structt, strdec, endd} =>
          newTabWithStyle tab (Indented, fn inner =>
            token structt
            ++ at inner
            ++ showStrDec inner strdec
            ++ cond inner {inactive = empty, active = at tab}
            ++ token endd)

      | Constraint {strexp, colon, sigexp} =>
          showStrExp tab strexp
          ++ token colon
          ++ showSigExpNewChild tab sigexp

      | FunAppExp {funid, lparen, strexp, rparen} =>
          (* The check for inserting a space after the `funid` is nice to
           * allow for both `F(G(X))` and `Fun (struct val x = 5 end)`.
           * (Personally, I like removing the space in the former case
           * and leaving the space in the second case, but this is certainly
           * open to debate.)
           *)
          newTab tab (fn inner =>
            token funid ++
            (if strExpInsideFunAppWantsSpaceBefore strexp then
               space
             else
               nospace) ++
            at inner ++ token lparen ++ nospace ++
            showStrExpNewChild inner strexp
            ++ nospace ++ token rparen)

      | FunAppDec {funid, lparen, strdec, rparen} =>
          (* See note above, about the maybe-space after `funid` *)
          newTab tab (fn inner =>
            token funid ++
            (if strDecInsideFunAppWantsSpaceBefore strdec then
               space
             else
               nospace) ++
            at inner ++ token lparen ++ nospace ++
            showStrDecNewChild inner strdec
            ++ nospace ++ token rparen)

      | LetInEnd {lett, strdec, inn, strexp, endd} =>
          showThingSimilarToLetInEnd tab
            ( lett
            , (decIsEmpty strdec, fn () => showStrDecNewChildWithStyle tab Indented strdec)
            , inn
            , (fn () => showStrExpNewChildWithStyle tab Indented strexp)
            , endd
            )

      (* | _ => text "<strexp>" *)

    end


  and showStrDec tab d =
    let
      open Ast.Str
    in
      case d of
        DecEmpty =>
          empty

      | DecCore d =>
          showDec tab d

      | DecStructure {structuree, elems, delims} =>
          newTab tab (fn tab =>
          let
            fun showConstraint constraint =
              case constraint of
                NONE => empty
              | SOME {colon, sigexp} =>
                  token colon
                  ++ (if sigExpWantsSameTabAsDec sigexp then
                        at tab ++ showSigExp tab sigexp
                      else
                        showSigExpNewChild tab sigexp)

            fun showOne (starter, {strid, constraint, eq, strexp}) =
              token starter
              ++ token strid
              ++ showConstraint constraint
              ++ token eq
              ++ (if strExpWantsSameTabAsDec strexp then
                    at tab ++ showStrExp tab strexp
                  else
                    showStrExpNewChild tab strexp)
          in
            at tab ++
            Seq.iterate op++
              (showOne (structuree, Seq.nth elems 0))
              (Seq.map (fn x => at tab ++ showOne x)
                (Seq.zip (delims, (Seq.drop elems 1))))
          end)


      | DecMultiple {elems, delims} =>
          let
            fun mk first (elem, delim) =
              (if first then empty else at tab)
              ++ showStrDec tab elem
              ++ showOption (fn d => nospace ++ token d) delim

            val things = Seq.zip (elems, delims)
          in
            Seq.iterate op++
              (mk true (Seq.nth things 0))
              (Seq.map (mk false) (Seq.drop things 1))
          end

      | DecLocalInEnd {locall, strdec1, inn, strdec2, endd} =>
          showThingSimilarToLetInEnd tab
            ( locall
            , (decIsEmpty strdec1, fn () => showStrDecNewChildWithStyle tab Indented strdec1)
            , inn
            , (fn () => showStrDecNewChildWithStyle tab Indented strdec2)
            , endd
            )

      (** This is MLton-specific. Useful for testing by parsing the entire
        * MLton implementation of the standard basis.
        *)
      | MLtonOverload {underscore, overload, prec, name, colon, ty, ass, elems, delims} =>
          newTab tab (fn inner =>
            let
              val front =
                at inner
                ++ token underscore ++ nospace ++ token overload
                ++ token prec
                ++ token name
                ++ token colon
                ++ showTyNewChild inner ty
                ++ at inner
                ++ token ass
                ++ token (MaybeLongToken.getToken (Seq.nth elems 0))

              fun showOne (d, e) =
                at inner ++ token d ++ token (MaybeLongToken.getToken e)
            in
              Seq.iterate op++
                front
                (Seq.zipWith showOne (delims, Seq.drop elems 1))
            end)

      (* | _ => text "<strdec>" *)
    end

end
