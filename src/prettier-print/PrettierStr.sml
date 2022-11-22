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
        Ident _ => false
      | _ => true
    end

  (* ====================================================================== *)

  fun showStrExpNewChild tab e = newTab tab (fn inner => showStrExp inner e)
  and showStrDecNewChild tab e = newTab tab (fn inner => showStrDec inner e)

  and showStrExp tab e =
    let
      open Ast.Str
    in
      case e of
        Ident id =>
          newTab tab (fn tab => at tab ++ token (MaybeLongToken.getToken id))

      | Struct {structt, strdec, endd} =>
          token structt ++ showStrDecNewChild tab strdec ++ token endd
(*

      | Constraint {strexp, colon, sigexp} =>
          showStrExp strexp
          ++ space ++ token colon
          \\ showSigExp sigexp

      | FunAppExp {funid, lparen, strexp, rparen} =>
          token funid
          \\ token lparen ++ showStrExp strexp ++ token rparen

      | FunAppDec {funid, lparen, strdec, rparen} =>
          token funid
          \\ token lparen ++ showStrDec strdec ++ token rparen

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
                  ++ (if sigExpWantsSameTabAsDec sigexp then at tab else empty)
                  ++ showSigExp tab sigexp

            fun showOne (starter, {strid, constraint, eq, strexp}) =
              token starter
              ++ token strid
              ++ showConstraint constraint
              ++ token eq
              ++ (if strExpWantsSameTabAsDec strexp then at tab else empty)
              ++ showStrExp tab strexp
          in
            Seq.iterate op++
              (showOne (structuree, Seq.nth elems 0))
              (Seq.map (fn x => at tab ++ showOne x)
                (Seq.zip (delims, (Seq.drop elems 1))))
          end


      | DecMultiple {elems, delims} =>
          let
            fun f i =
              showStrDec tab (Seq.nth elems i)
              ++
              (case Seq.nth delims i of
                NONE => empty
              | SOME sc => token sc)
          in
            if Seq.length elems = 0 then
              empty
            else
              Util.loop (1, Seq.length elems) (f 0)
              (fn (prev, i) => prev ++ at tab ++ f i)
          end

(*

      | DecLocalInEnd {locall, strdec1, inn, strdec2, endd} =>
          rigid (
            token locall
            $$
            indent (showStrDec strdec1)
            $$
            token inn
            $$
            indent (showStrDec strdec2)
            $$
            token endd
          )

      (** This is MLton-specific. Useful for testing by parsing the entire
        * MLton implementation of the standard basis.
        *)
      | MLtonOverload
        {underscore, overload, prec, name, colon, ty, ass, elems, delims} =>
          let
            val front =
              token underscore ++ token overload
              ++ space ++ token prec
              ++ space ++ token name
              ++ space ++ token colon
              ++ space ++ showTy ty
              ++ space ++ token ass

            fun showOne (d, e) =
              token d ++ space ++ token (MaybeLongToken.getToken e)
          in
            group (
              front
              $$
              indent (
                rigidVertically
                  (token (MaybeLongToken.getToken (Seq.nth elems 0)))
                  (Seq.zipWith showOne (delims, Seq.drop elems 1))
              )
            )
          end
*)
      | _ => text "<strdec>"
    end

end
