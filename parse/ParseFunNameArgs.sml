(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure ParseFunNameArgs:
sig
  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type tokens = Token.t Seq.t

  val fname_args: tokens
               -> InfixDict.t
               -> (int, Ast.Exp.fname_args) parser
end =
struct

  structure PC = ParserCombinators
  structure PS = ParseSimple
  structure PT = ParseTy
  structure PP = ParsePat
  exception Error = ParserUtils.Error

  structure Restriction = ExpPatRestriction

  type ('a, 'b) parser = ('a, 'b) ParserCombinators.parser
  type tokens = Token.t Seq.t


  fun fname_args toks infdict i =
    let
      val numToks = Seq.length toks
      fun tok i = Seq.nth toks i
      fun check f i = i < numToks andalso f (tok i)
      fun isReserved rc i =
        check (fn t => Token.Reserved rc = Token.getClass t) i

      (* fun isInfixedValueIdentifier i =
        check Token.isValueIdentifier i
        andalso InfixDict.contains infdict (tok i) *)

      (** The strategy we use here is to begin by parsing a sequence of atomic
        * patterns, which are terminated by either a colon (for the return type
        * annotation) or an equal (for the beginning of the function body).
        *
        * Once we have the patterns in hand, we can check to see if valid
        * function name and argument patterns can be extracted.
        *)

      val (i, pats) =
        PC.oneOrMoreWhile
          (fn i => not (isReserved Token.Colon i orelse isReserved Token.Equal i))
          (PP.pat toks infdict Restriction.At)
          i


      (* val maybeFirstName =
        case Seq.nth pats 0 of
          Ast.Pat.Parens {pat = Ast.Pat.Infix {id, ...}, ...} =>
            SOME id
        | Ast.Pat.Ident {id, ...} =>
            if not (Ast.MaybeLong.isLong id) then SOME id else NONE
        | _ => NONE

      val maybeSecondName =
        if Seq.length pats <= 1 then NONE else
        case Seq.nth pats 1 of
          Ast.Pat.Ident {id, ...} =>
            if
              not (Ast.MaybeLong.isLong id)
              andalso InfixDict.contains infdict (Ast.MaybeLong.getToken id)
            then
              SOME (Ast.MaybeLong.getToken id)
            else
              NONE

        | _ => NONE *)

      fun infixedFun () =
        if Seq.length pats <> 3 then
          ParserUtils.error
            { pos = Token.getSource (Ast.Pat.leftMostToken (Seq.nth pats 0))
            , what = "Invalid function definition."
            , explain = NONE
            }
        else
          case Seq.nth pats 1 of
            Ast.Pat.Ident {id, ...} =>
              if Ast.MaybeLong.isLong id then
                ParserUtils.error
                  { pos = Token.getSource (Ast.MaybeLong.getToken id)
                  , what = "Invalid function definition."
                  , explain = SOME "Could not find name of function."
                  }
              else
                Ast.Exp.InfixedFun
                  { larg = Seq.nth pats 0
                  , id = Ast.MaybeLong.getToken id
                  , rarg = Seq.nth pats 2
                  }
          | p =>
              ParserUtils.error
                { pos = Token.getSource (Ast.Pat.leftMostToken p)
                , what = "Invalid function definition."
                , explain = SOME "Could not find name of function."
                }

      fun prefixedFun () =
        case Seq.nth pats 0 of
          Ast.Pat.Ident {opp, id} =>
            if Ast.MaybeLong.isLong id then
              ParserUtils.error
                { pos = Token.getSource (Ast.MaybeLong.getToken id)
                , what = "Invalid function definition."
                , explain = SOME "Could not find name of function."
                }
            else
              Ast.Exp.PrefixedFun
                { opp = opp
                , id = Ast.MaybeLong.getToken id
                , args = Seq.drop pats 1
                }
        | p =>
            ParserUtils.error
              { pos = Token.getSource (Ast.Pat.leftMostToken p)
              , what = "Invalid function definition."
              , explain = SOME "Could not find name of function."
              }

      val result =
        case Seq.nth pats 0 of
          Ast.Pat.Parens
            {left=lp, right=rp, pat = Ast.Pat.Infix {left, id, right}}
            =>
            Ast.Exp.CurriedInfixedFun
              { lparen = lp
              , larg = left
              , id = id
              , rarg = right
              , rparen = rp
              , args = Seq.drop pats 1
              }

        | Ast.Pat.Ident {opp=NONE, id} =>
            if not (Ast.MaybeLong.isLong id)
               andalso InfixDict.contains infdict (Ast.MaybeLong.getToken id)
            then
              infixedFun ()
            else
              prefixedFun ()

        | _ =>
            prefixedFun ()
    in
      (i, result)
    end

end
