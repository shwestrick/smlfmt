(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierFun:
sig
  val showFunDec: Ast.Fun.fundec PrettierUtil.shower
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  open PrettierSig
  open PrettierStr
  infix 2 ++
  fun x ++ y = concat (x, y)

  (* ======================================================================= *)

  fun leftMostStrExp strexp =
    let
      open Ast.Str
    in
      case strexp of
        Constraint {strexp, ...} => leftMostStrExp strexp
      | _ => strexp
    end

  fun leftMostSigExp e =
    let
      open Ast.Sig
    in
      case e of
        WhereType {sigexp, ...} => leftMostSigExp sigexp
      | _ => e
    end

  fun sigExpWantsSameTabAsDec e =
    let
      open Ast.Sig
    in
      case leftMostSigExp e of
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

  (* ======================================================================= *)

  fun funArgWantsSpaceBefore fa =
    case fa of
      Ast.Fun.ArgSpec _ => true
    | Ast.Fun.ArgIdent _ => false

  fun showFunArg tab fa =
    case fa of
      Ast.Fun.ArgSpec spec => withNewChild showSpec tab spec
    | Ast.Fun.ArgIdent {strid, colon, sigexp} =>
        token strid ++ token colon ++ withNewChild showSigExp tab sigexp

  fun showFunDec tab (Ast.Fun.DecFunctor {functorr, elems, delims}) =
    let
      fun showFunctor
            first
            (starter, {funid, lparen, funarg, rparen, constraint, eq, strexp})
        =
          (if first then empty else at tab)
          ++ token starter ++ token funid
          ++ (if funArgWantsSpaceBefore funarg then
                space
              else
                nospace)
          ++ newTab tab (fn inner =>
              at inner
              ++ token lparen ++ nospace
              ++ showFunArg inner funarg ++ nospace
              ++ token rparen)
          ++ showOption
              (fn {colon, sigexp} =>
                token colon
                ++ (if sigExpWantsSameTabAsDec sigexp then
                      at tab ++ showSigExp tab sigexp
                    else
                      withNewChild showSigExp tab sigexp))
              constraint
          ++ token eq
          ++ (if strExpWantsSameTabAsDec strexp then
                at tab ++ showStrExp tab strexp
              else
                withNewChild showStrExp tab strexp)
    in
      Seq.iterate op++
        (showFunctor true (functorr, Seq.nth elems 0))
        (Seq.map (showFunctor false) (Seq.zip (delims, Seq.drop elems 1)))
    end

end
