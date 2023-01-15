(** Copyright (c) 2023 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure CompareAst:
sig
  val equal: Ast.t * Ast.t -> bool
end =
struct

  (* =======================================================================
   * combinators for functions of type 'a * 'a -> bool (equality checkers)
   *
   * makes it a bit easier to write equality checks over records with this
   * idiom:
   *
   *   val checker =
   *     at #field1 eq1 <&>
   *     at #field2 eq2 <&>
   *     ...            <&>
   *     at #fieldN eqN
   *
   *   val result = checker (x, y)
   *)

  type 'a eq = 'a * 'a -> bool

  fun at (select: 'b -> 'a) (eq: 'a eq) : 'b eq =
    fn (x: 'b, y: 'b) => eq (select x, select y)

  fun <&> (eq1: 'a eq, eq2: 'a eq) : 'a eq =
    fn (x, y) => eq1 (x, y) andalso eq2 (x, y)

  infix 2 <&>

  (* ======================================================================= *)

  fun equal_op eq (op1, op2) =
    case (op1, op2) of
      (SOME x, SOME y) => eq (x, y)
    | (NONE, NONE) => true
    | _ => false

  (* ======================================================================= *)

  open Ast


  fun equal_ty (t1, t2) = raise Fail "nyi"


  fun equal_exp (e1, e2) = raise Fail "nyi"


  and equal_dec (d1, d2) = raise Fail "nyi"


  fun equal_sigdec (sd1, sd2) = raise Fail "nyi"


  fun equal_sigexp (se1, se2) = raise Fail "nyi"


  fun equal_spec (s1, s2) = raise Fail "nyi"


  fun equal_strdec (sd1, sd2) =
    case (sd1, sd2) of
      (Str.DecEmpty, Str.DecEmpty) => true

    | (Str.DecCore d1, Str.DecCore d2) => equal_dec (d1, d2)

    | (Str.DecStructure s1, Str.DecStructure s2) =>
        let
          fun elem_equal (e1, e2) =
            let
              val checker =
                at #strid Token.same
                <&>
                at #constraint (equal_op
                  (at #colon Token.same <&> at #sigexp equal_sigexp))
                <&> at #eq Token.same <&> at #strexp equal_strexp
            in
              checker (e1, e2)
            end

          val checker =
            at #structuree Token.same <&> at #elems (Seq.equal elem_equal)
            <&> at #delims (Seq.equal Token.same)
        in
          checker (s1, s2)
        end

    | (Str.DecMultiple m1, Str.DecMultiple m2) =>
        let
          val checker =
            at #elems (Seq.equal equal_strdec)
            <&> at #delims (Seq.equal (equal_op Token.same))
        in
          checker (m1, m2)
        end

    | (Str.DecLocalInEnd lie1, Str.DecLocalInEnd lie2) =>
        let
          val checker =
            at #locall Token.same <&> at #strdec1 equal_strdec
            <&> at #inn Token.same <&> at #strdec2 equal_strdec
            <&> at #endd Token.same
        in
          checker (lie1, lie2)
        end

    | (Str.MLtonOverload o1, Str.MLtonOverload o2) =>
        let
          val checker =
            at #underscore Token.same <&> at #overload Token.same
            <&> at #prec Token.same <&> at #name Token.same
            <&> at #colon Token.same <&> at #colon Token.same
            <&> at #ty equal_ty <&> at #ass Token.same
            <&> at #elems (Seq.equal (at MaybeLongToken.getToken Token.same))
            <&> at #delims (Seq.equal Token.same)
        in
          checker (o1, o2)
        end

    | _ => false


  and equal_strexp (se1, se2) =
    case (se1, se2) of
      (Str.Ident i1, Str.Ident i2) =>
        at MaybeLongToken.getToken Token.same (i1, i2)

    | (Str.Struct s1, Str.Struct s2) =>
        let
          val checker =
            at #structt Token.same <&> at #strdec equal_strdec
            <&> at #endd Token.same
        in
          checker (s1, s2)
        end

    | (Str.Constraint c1, Str.Constraint c2) =>
        let
          val checker =
            at #strexp equal_strexp <&> at #colon Token.same
            <&> at #sigexp equal_sigexp
        in
          checker (c1, c2)
        end

    | (Str.FunAppExp e1, Str.FunAppExp e2) =>
        let
          val checker =
            at #funid Token.same <&> at #lparen Token.same
            <&> at #strexp equal_strexp <&> at #rparen Token.same
        in
          checker (e1, e2)
        end

    | (Str.FunAppDec d1, Str.FunAppDec d2) =>
        let
          val checker =
            at #funid Token.same <&> at #lparen Token.same
            <&> at #strdec equal_strdec <&> at #rparen Token.same
        in
          checker (d1, d2)
        end

    | (Str.LetInEnd lie1, Str.LetInEnd lie2) =>
        let
          val checker =
            at #lett Token.same <&> at #strdec equal_strdec
            <&> at #inn Token.same <&> at #strexp equal_strexp
            <&> at #endd Token.same
        in
          checker (lie1, lie2)
        end

    | _ => false


  fun equal_fundec (Fun.DecFunctor x, Fun.DecFunctor y) =
    let
      fun equal_funarg (fa1, fa2) =
        case (fa1, fa2) of
          (Fun.ArgSpec s1, Fun.ArgSpec s2) => equal_spec (s1, s2)

        | (Fun.ArgIdent i1, Fun.ArgIdent i2) =>
            let
              val checker =
                at #strid Token.same <&> at #colon Token.same
                <&> at #sigexp equal_sigexp
            in
              checker (i1, i2)
            end

        | _ => false

      fun equal_constraint (c1, c2) =
        equal_op (at #colon Token.same <&> at #sigexp equal_sigexp) (c1, c2)

      fun equal_elem (x, y) =
        let
          val checker =
            at #funid Token.same <&> at #lparen Token.same
            <&> at #funarg equal_funarg <&> at #rparen Token.same
            <&> at #constraint equal_constraint <&> at #eq Token.same
            <&> at #strexp equal_strexp
        in
          checker (x, y)
        end

      val checker =
        at #functorr Token.same <&> at #elems (Seq.equal equal_elem)
        <&> at #delims (Seq.equal Token.same)
    in
      checker (x, y)
    end


  fun equal_topexp (te1, te2) =
    let val checker = at #exp equal_exp <&> at #semicolon Token.same
    in checker (te1, te2)
    end


  fun equal_topdec (td1, td2) =
    case (td1, td2) of
      (SigDec sd1, SigDec sd2) => equal_sigdec (sd1, sd2)
    | (StrDec sd1, StrDec sd2) => equal_strdec (sd1, sd2)
    | (FunDec fd1, FunDec fd2) => equal_fundec (fd1, fd2)
    | (TopExp te1, TopExp te2) => equal_topexp (te1, te2)
    | _ => false


  fun equal (Ast tops1, Ast tops2) =
    let
      fun equal_topelem (te1, te2) =
        let
          val checker =
            at #topdec equal_topdec <&> at #semicolon (equal_op Token.same)
        in
          checker (te1, te2)
        end
    in
      Seq.equal equal_topelem (tops1, tops2)
    end

end
