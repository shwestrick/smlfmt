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

  fun showTy ty = PrettierTy.showTy ty
  fun showPat pat = PrettierPat.showPat pat
  fun showDec tab dec = PrettierExpAndDec.showDec tab dec
  fun showSpec tab spec = PrettierSig.showSpec tab spec
  fun showSigExp tab sigexp = PrettierSig.showSigExp tab sigexp
  fun showSigDec tab sigdec = PrettierSig.showSigDec tab sigdec

  (* ====================================================================== *)

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
      case e of
        Struct _ => true
      | _ => false
    end

  fun decIsEmpty e =
    case e of
      Ast.Str.DecEmpty => true
    | _ => false

  (* ====================================================================== *)

  fun showSigExpNewChild tab e = newTab tab (fn inner => at inner ++ showSigExp inner e)

  fun showStrExpNewChild tab e = newTab tab (fn inner => at inner ++ showStrExp inner e)
  and showStrDecNewChild tab e = newTab tab (fn inner => at inner ++ showStrDec inner e)

  and showStrExp tab e =
    let
      open Ast.Str
    in
      case e of
        Ident id =>
          token (MaybeLongToken.getToken id)
          (* newTab tab (fn tab => at tab ++ ) *)

      | Struct {structt, strdec, endd} =>
          token structt ++ showStrDecNewChild tab strdec ++ at tab ++ token endd

      | Constraint {strexp, colon, sigexp} =>
          showStrExp tab strexp
          ++ token colon
          ++ showSigExpNewChild tab sigexp

      | FunAppExp {funid, lparen, strexp, rparen} =>
          token funid ++ nospace ++ token lparen ++ nospace ++
          showStrExp tab strexp ++ nospace ++ token rparen

      | FunAppDec {funid, lparen, strdec, rparen} =>
          token funid ++ nospace ++ token lparen ++ nospace ++
          newTab tab (fn inner =>
            at inner ++
            showStrDec inner strdec ++ nospace ++ token rparen)
(*

      | LetInEnd {lett, strdec, inn, strexp, endd} =>
          let
            val topPart =
              token lett
              $$
              indent (showStrDec strdec)
              $$
              token inn

            val topPart =
              if isMultipleDecs strdec then
                topPart
              else
                group topPart
          in
            group (
              topPart
              $$
              indent (group (showStrExp strexp))
              $$
              token endd
            )
          end
*)
      | _ => text "<strexp>"

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
            Seq.iterate op++
              (showOne (structuree, Seq.nth elems 0))
              (Seq.map (fn x => at tab ++ showOne x)
                (Seq.zip (delims, (Seq.drop elems 1))))
          end


      | DecMultiple {elems, delims} =>
          let
            fun mk (elem, delim) =
              at tab ++ showStrDec tab elem
              ++ showOption (fn d => nospace ++ token d) delim
          in
            Seq.iterate op++ empty (Seq.zipWith mk (elems, delims))
          end

      | DecLocalInEnd {locall, strdec1, inn, strdec2, endd} =>
          showThingSimilarToLetInEnd tab
            ( locall
            , (decIsEmpty strdec1, fn () => showStrDecNewChild tab strdec1)
            , inn
            , (fn () => showStrDecNewChild tab strdec2)
            , endd
            )

      (** This is MLton-specific. Useful for testing by parsing the entire
        * MLton implementation of the standard basis.
        *)
      | MLtonOverload {underscore, overload, prec, name, colon, ty, ass, elems, delims} =>
          newTab tab (fn tab =>
            let
              val front =
                at tab
                ++ token underscore ++ nospace ++ token overload
                ++ token prec
                ++ token name
                ++ token colon
                ++ showTy ty
                ++ at tab
                ++ token ass
                ++ token (MaybeLongToken.getToken (Seq.nth elems 0))

              fun showOne (d, e) =
                at tab ++ token d ++ token (MaybeLongToken.getToken e)
            in
              Seq.iterate op++
                front
                (Seq.zipWith showOne (delims, Seq.drop elems 1))
            end)

      (* | _ => text "<strdec>" *)
    end

end
