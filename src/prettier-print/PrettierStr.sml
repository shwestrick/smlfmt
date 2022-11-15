(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierStr:
sig
  type doc = TabbedStringDoc.tab TabbedTokenDoc.t
  type tab = TabbedStringDoc.tab TabbedTokenDoc.tab
  val showStrExp: Ast.Str.strexp -> doc
  val showStrExpAt: tab -> Ast.Str.strexp -> doc
  val showStrDec: Ast.Str.strdec -> doc
  val showStrDecAt: tab -> Ast.Str.strdec -> doc
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  infix 2 ++
  type doc = TabbedStringDoc.tab TabbedTokenDoc.t
  type tab = TabbedStringDoc.tab TabbedTokenDoc.tab

  fun showTy ty = PrettierTy.showTy ty
  fun showPat pat = PrettierPat.showPat pat
  fun showExp exp = PrettierExpAndDec.showExp exp
  fun showDec dec = PrettierExpAndDec.showDec dec
  fun showSpec spec = PrettierSig.showSpec spec
  fun showSigExp sigexp = PrettierSig.showSigExp sigexp
  fun showSigExpAt tab sigexp = PrettierSig.showSigExpAt tab sigexp
  fun showSigDec sigdec = PrettierSig.showSigDec sigdec


  fun showStrExp _ = text "<strexp>"
  and showStrExpAt tab _ = text "<strexp>"

  and showStrDec d = newTab (fn tab => showStrDecAt tab d)

  and showStrDecAt tab d =
    let
      open Ast.Str
    in
      case d of
        DecEmpty =>
          empty

      | DecCore d =>
          showDec d

      | DecStructure {structuree, elems, delims} =>
          let
            fun showConstraint constraint =
              case constraint of
                NONE => empty
              | SOME {colon, sigexp} =>
                  token colon ++ breakspace tab ++ showSigExpAt tab sigexp

            fun showOne (starter, {strid, constraint, eq, strexp}) =
              separateWithSpaces
                [ SOME (token starter)
                , SOME (token strid)
                ]
              ++ showConstraint constraint
              ++ space ++ token eq
              ++ breakspace tab
              ++ showStrExpAt tab strexp
          in
            Seq.iterate op++ 
              (showOne (structuree, Seq.nth elems 0))
              (Seq.map (fn x => break tab ++ showOne x)
                (Seq.zip (delims, (Seq.drop elems 1))))
          end


      | DecMultiple {elems, delims} =>
          let
            fun f i =
              showStrDecAt tab (Seq.nth elems i)
              ++
              (case Seq.nth delims i of
                NONE => empty
              | SOME sc => token sc)
          in
            if Seq.length elems = 0 then
              empty
            else
              Util.loop (1, Seq.length elems) (f 0)
              (fn (prev, i) => prev ++ break tab ++ f i)
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
