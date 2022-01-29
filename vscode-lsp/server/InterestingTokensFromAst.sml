structure InterestingTokensFromAst:
sig
  datatype info =
    Constructor
  | Function
  | InfixOp
  | StructureId
  | Label
  | SpecialKeyword (* fuzzy... *)
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
  | StructureId
  | Label
  | SpecialKeyword
  | NotInteresting

  type acc = (Token.t * info) list


  fun typbind (acc, {elems = mutuals, ...}: Ast.Exp.typbind) =
    let
    in
      acc
    end


  fun datbind (acc, {elems = mutuals, ...}: Ast.Exp.datbind) =
    let
      fun variant (acc, {id, ...}) =
        (id, Constructor) :: acc

      fun mutualdat (acc, {elems = variants, ...}) =
        Seq.iterate variant acc variants
    in
      Seq.iterate mutualdat acc mutuals
    end

  fun pat (acc: acc, p) : acc =
    let
      open Ast.Pat
    in
      case p of
        List {elems, ...} =>
          Seq.iterate pat acc elems
      | Tuple {elems, ...} =>
          Seq.iterate pat acc elems
      | Parens {pat=p, ...} =>
          pat (acc, p)
      | Con {id, atpat, ...} =>
          (MaybeLongToken.getToken id, Constructor) :: pat (acc, atpat)
      | Infix {left, id, right} =>
          (id, Constructor) :: pat (pat (acc, left), right)
      | Typed {pat=p, ...} =>
          pat (acc, p)
      | Layered {pat=p, ...} =>
          pat (acc, p)
      | Record {elems, ...} =>
          let
            fun patrow (acc, pr) =
              case pr of
                LabEqPat {lab, pat=p, ...} =>
                  (lab, Label) :: pat (acc, p)
              | LabAsPat {id, aspat = SOME {pat=p, ...}, ...} =>
                  (id, Label) :: pat (acc, p)
              | LabAsPat {id, aspat = NONE, ...} =>
                  (id, Label) :: acc
              | _ => acc
          in
            Seq.iterate patrow acc elems
          end
      | _ => acc
    end


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
            fun clause (acc, {pat=p, exp=e', ...}) =
              pat (exp (acc, e'), p)
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
            fun clause (acc, {pat=p, exp=e', ...}) =
              pat (exp (acc, e'), p)
          in
            Seq.iterate clause (exp (acc, e')) clauses
          end

      | Fn {elems = clauses, ...} =>
          let
            fun clause (acc, {pat=p, exp=e', ...}) =
              pat (exp (acc, e'), p)
          in
            Seq.iterate clause acc clauses
          end

      | Record {elems, ...} =>
          let
            fun patrow (acc, {lab, exp=e, ...}) =
              (lab, Label) :: exp (acc, e)
          in
            Seq.iterate patrow acc elems
          end

      | _ => acc
    end


  and dec (acc, d) =
    let
      open Ast.Exp
    in
      case d of
        DecFun {fvalbind = {elems = mutuals, ...}, ...} =>
          let
            fun fname_args (acc, fna) =
              case fna of
                PrefixedFun {id, args, ...} =>
                  (id, Function) :: Seq.iterate pat acc args
              | InfixedFun {id, larg, rarg} =>
                  (id, Function) :: pat (pat (acc, larg), rarg)
              | CurriedInfixedFun {id, larg, rarg, args, ...} =>
                  (id, Function) :: Seq.iterate pat (pat (pat (acc, larg), rarg)) args

            fun clause (acc, {fname_args=fna, exp=e, ...}) =
              fname_args (exp (acc, e), fna)

            fun mutualfunc (acc, {elems = clauses, ...}) =
              Seq.iterate clause acc clauses
          in
            Seq.iterate mutualfunc acc mutuals
          end

      | DecVal {elems = mutuals, ...} =>
          let
            fun mutualval (acc, {pat=p, exp=e, ...}) =
              pat (exp (acc, e), p)
          in
            Seq.iterate mutualval acc mutuals
          end

      | DecLocal {left_dec, right_dec, ...} =>
          dec (dec (acc, left_dec), right_dec)

      | DecMultiple {elems, ...} =>
          Seq.iterate dec acc elems

      | DecInfix {elems, ...} =>
          Seq.toList (Seq.map (fn t => (t, InfixOp)) elems) @ acc

      | DecInfixr {elems, ...} =>
          Seq.toList (Seq.map (fn t => (t, InfixOp)) elems) @ acc

      | DecNonfix {elems, ...} =>
          Seq.toList (Seq.map (fn t => (t, InfixOp)) elems) @ acc

      | DecDatatype {datbind=db, withtypee = SOME {typbind = tb, ...}, ...} =>
          datbind (typbind (acc, tb), db)

      | DecDatatype {datbind=db, withtypee = NONE, ...} =>
          datbind (acc, db)

      | DecOpen {elems, ...} =>
          let
            fun elem (acc, t) =
              (MaybeLongToken.getToken t, StructureId) :: acc
          in
            Seq.iterate elem acc elems
          end

      | _ => acc
    end


  fun sigexp (acc, e) =
    let
      open Ast.Sig
    in
      case e of
        Ident x => (x, StructureId) :: acc
      | WhereType {sigexp=se, ...} => sigexp (acc, se)
      | Spec {sigg, spec=s, endd} =>
          (sigg, SpecialKeyword) :: (endd, SpecialKeyword) :: spec (acc, s)
    end


  and spec (acc, s) =
    let
      open Ast.Sig
    in
      case s of
        Multiple {elems, ...} =>
          Seq.iterate spec acc elems
      | Datatype {elems = mutuals, ...} =>
          let
            fun clause (acc, {vid, ...}) =
              (vid, Constructor) :: acc
            fun mutualdat (acc, {elems = clauses, ...}) =
              Seq.iterate clause acc clauses
          in
            Seq.iterate mutualdat acc mutuals
          end
      | Structure {elems = mutuals, ...} =>
          let
            fun mutualstr (acc, {sigexp=se, ...}) =
              sigexp (acc, se)
          in
            Seq.iterate mutualstr acc mutuals
          end
      | _ => acc
    end


  fun strexp (acc, e) =
    case e of
      Ast.Str.Ident x =>
        (MaybeLongToken.getToken x, StructureId) :: acc
    | Ast.Str.Struct {structt, strdec=sd, endd} =>
        (structt, SpecialKeyword) :: (endd, SpecialKeyword) :: strdec (acc, sd)
    | Ast.Str.Constraint {strexp=se, ...} =>
        strexp (acc, se)
    | Ast.Str.FunAppExp {funid, strexp=se, ...} =>
        (funid, StructureId) :: strexp (acc, se)
    | Ast.Str.FunAppDec {funid, strdec=sd, ...} =>
        (funid, StructureId) :: strdec (acc, sd)
    | Ast.Str.LetInEnd {strdec=sd, strexp=se, ...} =>
        strexp (strdec (acc, sd), se)
    (* | _ => acc *)


  and strdec (acc, d) =
    case d of
      Ast.Str.DecMultiple {elems, ...} =>
        Seq.iterate strdec acc elems
    | Ast.Str.DecCore cd => dec (acc, cd)
    | Ast.Str.DecStructure {elems = mutuals, ...} =>
        let
          fun mutualstruct (acc, {strid, strexp=se, constraint, ...}) =
            case constraint of
              NONE => (strid, StructureId) :: strexp (acc, se)
            | SOME {sigexp=sige, ...} =>
                (strid, StructureId) :: strexp (sigexp (acc, sige), se)
        in
          Seq.iterate mutualstruct acc mutuals
        end
    | Ast.Str.DecLocalInEnd {strdec1, strdec2, ...} =>
        strdec (strdec (acc, strdec1), strdec2)
    | _ => acc


  fun fundec (acc, d) =
    case d of
      Ast.Fun.DecFunctor {elems = mutuals, ...} =>
        let
          fun mutualfun (acc, {funid, strexp=se, ...}) =
            (funid, StructureId) :: strexp (acc, se)
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
