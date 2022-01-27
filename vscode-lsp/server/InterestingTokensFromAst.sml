structure InterestingTokensFromAst:
sig
  datatype info =
    Constructor
  | Function
  | InfixOp
  | NotInteresting (* defer to lexer token class *)

  (** Pull out some interesting tokens from the full parse tree. Doesn't
    * have to get all them; the result will be merged with the lexed tokens,
    * removing duplicates (and prefering the more specific info from this
    * function in those cases).
    *)
  val extract: Ast.t -> (Token.t * info) Seq.t
end =
struct

  datatype info =
    Constructor
  | Function
  | InfixOp
  | NotInteresting

  type acc = (Token.t * info) list

  fun exp (acc: acc, e) : acc =
    let
      open Ast.Exp
    in
      case e of
        Tuple {elems, ...} =>
          Seq.iterate exp acc elems

      | List {elems, ...} =>
          Seq.iterate exp acc elems

      | Sequence {elems, ...} =>
          Seq.iterate exp acc elems

      | LetInEnd {dec=d, exps, ...} =>
          Seq.iterate exp (dec (acc, d)) exps

      | Parens {exp=e', ...} => exp (acc, e')

      | App {left, right} => exp (exp (acc, left), right)

      | Infix {left, id, right} =>
          (id, InfixOp) :: exp (exp (acc, left), right)

      | Typed {exp=e', ...} => exp (acc, e')

      | Andalso {left, right, ...} => exp (exp (acc, left), right)

      | Orelse {left, right, ...} => exp (exp (acc, left), right)

      | Handle {exp=e', elems = clauses, ...} =>
          let
            fun clause (acc, {exp=e', ...}) = exp (acc, e')
          in
            Seq.iterate clause (exp (acc, e')) clauses
          end

      | Raise {exp=e', ...} => exp (acc, e')

      | IfThenElse {exp1, exp2, exp3, ...} =>
          exp (exp (exp (acc, exp1), exp2), exp3)

      | While {exp1, exp2, ...} =>
          exp (exp (acc, exp1), exp2)

      | Case {exp=e', elems = clauses, ...} =>
          let
            fun clause (acc, {exp=e', ...}) = exp (acc, e')
          in
            Seq.iterate clause (exp (acc, e')) clauses
          end

      | Fn {elems = clauses, ...} =>
          let
            fun clause (acc, {exp=e', ...}) = exp (acc, e')
          in
            Seq.iterate clause acc clauses
          end

      | _ => acc
    end


  and dec (acc, d) =
    case d of
      Ast.Exp.DecFun {fvalbind = {elems = mutuals, ...}, ...} =>
        let
          fun fname fname_args =
            case fname_args of
              Ast.Exp.PrefixedFun {id, ...} => id
            | Ast.Exp.InfixedFun {id, ...} => id
            | Ast.Exp.CurriedInfixedFun {id, ...} => id

          fun clause (acc, {fname_args, exp=e, ...}) =
            (fname fname_args, Function) :: exp (acc, e)

          fun mutualfunc (acc, {elems = clauses, ...}) =
            Seq.iterate clause acc clauses
        in
          Seq.iterate mutualfunc acc mutuals
        end

    | Ast.Exp.DecVal {elems = mutuals, ...} =>
        let
          fun mutualval (acc, {exp=e, ...}) =
            exp (acc, e)
        in
          Seq.iterate mutualval acc mutuals
        end

    | Ast.Exp.DecLocal {left_dec, right_dec, ...} =>
        dec (dec (acc, left_dec), right_dec)

    | Ast.Exp.DecMultiple {elems, ...} =>
        Seq.iterate dec acc elems

    | Ast.Exp.DecInfix {elems, ...} =>
        Seq.toList (Seq.map (fn t => (t, InfixOp)) elems) @ acc

    | Ast.Exp.DecInfixr {elems, ...} =>
        Seq.toList (Seq.map (fn t => (t, InfixOp)) elems) @ acc

    | Ast.Exp.DecNonfix {elems, ...} =>
        Seq.toList (Seq.map (fn t => (t, InfixOp)) elems) @ acc

    | _ => acc


  fun strexp (acc, e) =
    case e of
      Ast.Str.Struct {strdec=sd, ...} =>
        strdec (acc, sd)
    | Ast.Str.Constraint {strexp=se, ...} =>
        strexp (acc, se)
    | Ast.Str.FunAppExp {strexp=se, ...} =>
        strexp (acc, se)
    | Ast.Str.FunAppDec {strdec=sd, ...} =>
        strdec (acc, sd)
    | Ast.Str.LetInEnd {strdec=sd, strexp=se, ...} =>
        strexp (strdec (acc, sd), se)
    | _ => acc


  and strdec (acc, d) =
    case d of
      Ast.Str.DecMultiple {elems, ...} =>
        Seq.iterate strdec acc elems
    | Ast.Str.DecCore cd => dec (acc, cd)
    | Ast.Str.DecStructure {elems = mutuals, ...} =>
        let
          fun mutualstruct (acc, {strexp=se, ...}) =
            strexp (acc, se)
        in
          Seq.iterate mutualstruct acc mutuals
        end
    | _ => acc


  fun fundec (acc, d) =
    case d of
      Ast.Fun.DecFunctor {elems = mutuals, ...} =>
        let
          fun mutualfun (acc, {strexp=se, ...}) =
            strexp (acc, se)
        in
          Seq.iterate mutualfun acc mutuals
        end


  fun extract (Ast.Ast topdecs) =
    let
      fun topdec (acc, {topdec=d, ...}) =
        case d of
          Ast.StrDec sd => strdec (acc, sd)
        | Ast.FunDec fd => fundec (acc, fd)
        | _ => acc

      val elems = Seq.iterate topdec [] topdecs
    in
      Seq.fromList elems
    end

end
