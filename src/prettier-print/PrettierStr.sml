(** Copyright (c) 2022 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettierStr:
sig
  val showStrExp: Ast.Str.strexp PrettierUtil.shower
  val showStrDec: Ast.Str.strdec PrettierUtil.shower
end =
struct

  open TabbedTokenDoc
  open PrettierUtil
  open PrettierTy
  open PrettierExpAndDec
  open PrettierSig
  infix 2 ++
  fun x ++ y = concat (x, y)

  (* ====================================================================== *)

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

  fun showStrExp tab e =
    let
      open Ast.Str
    in
      case e of
        Ident id =>
          token (MaybeLongToken.getToken id)
          (* newTab tab (fn tab => goto tab ++ ) *)

      | Struct {structt, strdec, endd} =>
          if decIsEmpty strdec then
            token structt ++ goto tab ++ token endd
          else
            newTabWithStyle tab (Indented, fn inner =>
              token structt
              ++ goto inner
              ++ showStrDec inner strdec
              ++ cond inner {inactive = empty, active = goto tab}
              ++ token endd)

      | Constraint {strexp, colon, sigexp} =>
          showStrExp tab strexp
          ++ token colon
          ++ withNewChild showSigExp tab sigexp

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
            goto inner ++ token lparen ++ nospace ++
            withNewChild showStrExp inner strexp
            ++ nospace ++ token rparen)

      | FunAppDec {funid, lparen, strdec, rparen} =>
          (* See note above, about the maybe-space after `funid` *)
          newTab tab (fn inner =>
            token funid ++
            (if strDecInsideFunAppWantsSpaceBefore strdec then
               space
             else
               nospace) ++
            goto inner ++ token lparen ++ nospace ++
            withNewChild showStrDec inner strdec
            ++ nospace ++ token rparen)

      | LetInEnd {lett, strdec, inn, strexp, endd} =>
          showThingSimilarToLetInEnd tab
            { lett = lett
            , isEmpty1 = decIsEmpty strdec
            , doc1 = withNewChildWithStyle Indented showStrDec tab strdec
            , inn = inn
            , doc2 = withNewChildWithStyle Indented showStrExp tab strexp
            , endd = endd
            }
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
                  (if Token.getClass colon = Token.Reserved Token.ColonArrow then
                     space
                   else
                     nospace)
                  ++ token colon
                  ++ (if sigExpWantsSameTabAsDec sigexp then
                        goto tab ++ showSigExp tab sigexp
                      else
                        withNewChild showSigExp tab sigexp)

            fun showOne (starter, {strid, constraint, eq, strexp}) =
              token starter
              ++ token strid
              ++ showConstraint constraint
              ++ token eq
              ++ (if strExpWantsSameTabAsDec strexp then
                    goto tab ++ showStrExp tab strexp
                  else
                    withNewChild showStrExp tab strexp)
          in
            goto tab ++
            Seq.iterate op++
              (showOne (structuree, Seq.nth elems 0))
              (Seq.map (fn x => goto tab ++ showOne x)
                (Seq.zip (delims, (Seq.drop elems 1))))
          end)


      | DecMultiple {elems, delims} =>
          let
            fun mk first (elem, delim) =
              (if first then empty else goto tab)
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
            { lett = locall
            , isEmpty1 = decIsEmpty strdec1
            , doc1 = withNewChildWithStyle Indented showStrDec tab strdec1
            , inn = inn
            , doc2 = withNewChildWithStyle Indented showStrDec tab strdec2
            , endd = endd
            }

      (** This is MLton-specific. Useful for testing by parsing the entire
        * MLton implementation of the standard basis.
        *)
      | MLtonOverload {underscore, overload, prec, name, colon, ty, ass, elems, delims} =>
          newTab tab (fn inner =>
            let
              val front =
                goto inner
                ++ token underscore ++ nospace ++ token overload
                ++ token prec
                ++ token name
                ++ token colon
                ++ withNewChild showTy inner ty
                ++ goto inner
                ++ token ass
                ++ token (MaybeLongToken.getToken (Seq.nth elems 0))

              fun showOne (d, e) =
                goto inner ++ token d ++ token (MaybeLongToken.getToken e)
            in
              Seq.iterate op++
                front
                (Seq.zipWith showOne (delims, Seq.drop elems 1))
            end)

      (* | _ => text "<strdec>" *)
    end

end
