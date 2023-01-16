(** Copyright (c) 2023 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure CompareAst:
sig
  val equal: {tabWidth: int} -> Ast.t * Ast.t -> bool
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


  fun equal {tabWidth: int} (Ast tops1, Ast tops2) =
    let

      fun equal_tok (t1, t2) =
        Token.sameExceptForMultilineIndentation {tabWidth = tabWidth} (t1, t2)


      fun equal_syntaxseq eq (s1, s2) = raise Fail "nyi"


      fun equal_ty (t1, t2) = raise Fail "nyi"


      fun equal_exp (e1, e2) =
        case (e1, e2) of
          (Exp.Const c1, Exp.Const c2) => equal_tok (c1, c2)
        | _ => raise Fail "nyi"


      and equal_dec (d1, d2) = raise Fail "nyi"


      fun equal_sigdec (Sig.Signature s1, Sig.Signature s2) =
        let
          val checker =
            at #signaturee equal_tok <&> at #delims (Seq.equal equal_tok)
            <&>
            at #elems (Seq.equal
              (at #ident equal_tok <&> at #eq equal_tok
               <&> at #sigexp equal_sigexp))
        in
          checker (s1, s2)
        end


      and equal_sigexp (se1, se2) =
        case (se1, se2) of
          (Sig.Ident i1, Sig.Ident i2) => equal_tok (i1, i2)

        | (Sig.Spec s1, Sig.Spec s2) =>
            let
              val checker =
                at #sigg equal_tok <&> at #spec equal_spec
                <&> at #endd equal_tok
            in
              checker (s1, s2)
            end

        | (Sig.WhereType w1, Sig.WhereType w2) =>
            let
              val checker =
                at #sigexp equal_sigexp
                <&>
                at #elems (Seq.equal
                  (at #wheree equal_tok <&> at #typee equal_tok
                   <&> at #tyvars (equal_syntaxseq equal_tok)
                   <&> at #tycon (at MaybeLongToken.getToken equal_tok)
                   <&> at #eq equal_tok <&> at #ty equal_ty))
            in
              checker (w1, w2)
            end

        | _ => false


      and equal_spec (s1, s2) =
        case (s1, s2) of
          (Sig.EmptySpec, Sig.EmptySpec) => true

        | (Sig.Val v1, Sig.Val v2) =>
            let
              val checker =
                at #vall equal_tok
                <&>
                at #elems (Seq.equal
                  (at #vid equal_tok <&> at #colon equal_tok <&> at #ty equal_ty))
                <&> at #delims (Seq.equal equal_tok)
            in
              checker (v1, v2)
            end

        | (Sig.Type t1, Sig.Type t2) =>
            let
              val checker =
                at #typee equal_tok
                <&>
                at #elems (Seq.equal
                  (at #tyvars (equal_syntaxseq equal_tok)
                   <&> at #tycon equal_tok))
                <&> at #delims (Seq.equal equal_tok)
            in
              checker (t1, t2)
            end

        | (Sig.TypeAbbreviation a1, Sig.TypeAbbreviation a2) =>
            let
              val checker =
                at #typee equal_tok
                <&>
                at #elems (Seq.equal
                  (at #tyvars (equal_syntaxseq equal_tok)
                   <&> at #tycon equal_tok <&> at #eq equal_tok
                   <&> at #ty equal_ty)) <&> at #delims (Seq.equal equal_tok)
            in
              checker (a1, a2)
            end

        | (Sig.Eqtype e1, Sig.Eqtype e2) =>
            let
              val checker =
                at #eqtypee equal_tok
                <&>
                at #elems (Seq.equal
                  (at #tyvars (equal_syntaxseq equal_tok)
                   <&> at #tycon equal_tok))
                <&> at #delims (Seq.equal equal_tok)
            in
              checker (e1, e2)
            end

        | (Sig.Datatype d1, Sig.Datatype d2) =>
            let
              val checker =
                at #datatypee equal_tok <&> at #delims (Seq.equal equal_tok)
                <&>
                at #elems (Seq.equal
                  (at #tyvars (equal_syntaxseq equal_tok)
                   <&> at #tycon equal_tok <&> at #eq equal_tok
                   <&> at #optbar (equal_op equal_tok)
                   <&> at #delims (Seq.equal equal_tok)
                   <&>
                   at #elems (Seq.equal
                     (at #vid equal_tok
                      <&>
                      at #arg (equal_op (at #off equal_tok <&> at #ty equal_ty))))))
            in
              checker (d1, d2)
            end

        | (Sig.ReplicateDatatype r1, Sig.ReplicateDatatype r2) =>
            let
              val checker =
                at #left_datatypee equal_tok <&> at #left_id equal_tok
                <&> at #eq equal_tok <&> at #right_datatypee equal_tok
                <&> at #right_id (at MaybeLongToken.getToken equal_tok)
            in
              checker (r1, r2)
            end

        | (Sig.Exception e1, Sig.Exception e2) =>
            let
              val checker =
                at #exceptionn equal_tok <&> at #delims (Seq.equal equal_tok)
                <&>
                at #elems (Seq.equal
                  (at #vid equal_tok
                   <&>
                   at #arg (equal_op (at #off equal_tok <&> at #ty equal_ty))))
            in
              checker (e1, e2)
            end

        | (Sig.Structure s1, Sig.Structure s2) =>
            let
              val checker =
                at #structuree equal_tok <&> at #delims (Seq.equal equal_tok)
                <&>
                at #elems (Seq.equal
                  (at #id equal_tok <&> at #colon equal_tok
                   <&> at #sigexp equal_sigexp))
            in
              checker (s1, s2)
            end

        | (Sig.Include i1, Sig.Include i2) =>
            let val checker = at #includee equal_tok <&> at #sigexp equal_sigexp
            in checker (i1, i2)
            end

        | (Sig.IncludeIds i1, Sig.IncludeIds i2) =>
            let
              val checker =
                at #includee equal_tok <&> at #sigids (Seq.equal equal_tok)
            in
              checker (i1, i2)
            end

        | (Sig.SharingType s1, Sig.SharingType s2) =>
            let
              val checker =
                at #spec equal_spec <&> at #sharingg equal_tok
                <&> at #typee equal_tok
                <&> at #elems (Seq.equal (at MaybeLongToken.getToken equal_tok))
                <&> at #delims (Seq.equal equal_tok)
            in
              checker (s1, s2)
            end

        | (Sig.Sharing s1, Sig.Sharing s2) =>
            let
              val checker =
                at #spec equal_spec <&> at #sharingg equal_tok
                <&> at #elems (Seq.equal (at MaybeLongToken.getToken equal_tok))
                <&> at #delims (Seq.equal equal_tok)
            in
              checker (s1, s2)
            end

        | (Sig.Multiple m1, Sig.Multiple m2) =>
            let
              val checker =
                at #elems (Seq.equal equal_spec)
                <&> at #delims (Seq.equal (equal_op equal_tok))
            in
              checker (m1, m2)
            end

        | _ => false


      fun equal_strdec (sd1, sd2) =
        case (sd1, sd2) of
          (Str.DecEmpty, Str.DecEmpty) => true

        | (Str.DecCore d1, Str.DecCore d2) => equal_dec (d1, d2)

        | (Str.DecStructure s1, Str.DecStructure s2) =>
            let
              fun elem_equal (e1, e2) =
                let
                  val checker =
                    at #strid equal_tok
                    <&>
                    at #constraint (equal_op
                      (at #colon equal_tok <&> at #sigexp equal_sigexp))
                    <&> at #eq equal_tok <&> at #strexp equal_strexp
                in
                  checker (e1, e2)
                end

              val checker =
                at #structuree equal_tok <&> at #elems (Seq.equal elem_equal)
                <&> at #delims (Seq.equal equal_tok)
            in
              checker (s1, s2)
            end

        | (Str.DecMultiple m1, Str.DecMultiple m2) =>
            let
              val checker =
                at #elems (Seq.equal equal_strdec)
                <&> at #delims (Seq.equal (equal_op equal_tok))
            in
              checker (m1, m2)
            end

        | (Str.DecLocalInEnd lie1, Str.DecLocalInEnd lie2) =>
            let
              val checker =
                at #locall equal_tok <&> at #strdec1 equal_strdec
                <&> at #inn equal_tok <&> at #strdec2 equal_strdec
                <&> at #endd equal_tok
            in
              checker (lie1, lie2)
            end

        | (Str.MLtonOverload o1, Str.MLtonOverload o2) =>
            let
              val checker =
                at #underscore equal_tok <&> at #overload equal_tok
                <&> at #prec equal_tok <&> at #name equal_tok
                <&> at #colon equal_tok <&> at #colon equal_tok
                <&> at #ty equal_ty <&> at #ass equal_tok
                <&> at #elems (Seq.equal (at MaybeLongToken.getToken equal_tok))
                <&> at #delims (Seq.equal equal_tok)
            in
              checker (o1, o2)
            end

        | _ => false


      and equal_strexp (se1, se2) =
        case (se1, se2) of
          (Str.Ident i1, Str.Ident i2) =>
            at MaybeLongToken.getToken equal_tok (i1, i2)

        | (Str.Struct s1, Str.Struct s2) =>
            let
              val checker =
                at #structt equal_tok <&> at #strdec equal_strdec
                <&> at #endd equal_tok
            in
              checker (s1, s2)
            end

        | (Str.Constraint c1, Str.Constraint c2) =>
            let
              val checker =
                at #strexp equal_strexp <&> at #colon equal_tok
                <&> at #sigexp equal_sigexp
            in
              checker (c1, c2)
            end

        | (Str.FunAppExp e1, Str.FunAppExp e2) =>
            let
              val checker =
                at #funid equal_tok <&> at #lparen equal_tok
                <&> at #strexp equal_strexp <&> at #rparen equal_tok
            in
              checker (e1, e2)
            end

        | (Str.FunAppDec d1, Str.FunAppDec d2) =>
            let
              val checker =
                at #funid equal_tok <&> at #lparen equal_tok
                <&> at #strdec equal_strdec <&> at #rparen equal_tok
            in
              checker (d1, d2)
            end

        | (Str.LetInEnd lie1, Str.LetInEnd lie2) =>
            let
              val checker =
                at #lett equal_tok <&> at #strdec equal_strdec
                <&> at #inn equal_tok <&> at #strexp equal_strexp
                <&> at #endd equal_tok
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
                    at #strid equal_tok <&> at #colon equal_tok
                    <&> at #sigexp equal_sigexp
                in
                  checker (i1, i2)
                end

            | _ => false

          fun equal_constraint (c1, c2) =
            equal_op (at #colon equal_tok <&> at #sigexp equal_sigexp) (c1, c2)

          fun equal_elem (x, y) =
            let
              val checker =
                at #funid equal_tok <&> at #lparen equal_tok
                <&> at #funarg equal_funarg <&> at #rparen equal_tok
                <&> at #constraint equal_constraint <&> at #eq equal_tok
                <&> at #strexp equal_strexp
            in
              checker (x, y)
            end

          val checker =
            at #functorr equal_tok <&> at #elems (Seq.equal equal_elem)
            <&> at #delims (Seq.equal equal_tok)
        in
          checker (x, y)
        end


      fun equal_topexp (te1, te2) =
        let val checker = at #exp equal_exp <&> at #semicolon equal_tok
        in checker (te1, te2)
        end


      fun equal_topdec (td1, td2) =
        case (td1, td2) of
          (SigDec sd1, SigDec sd2) => equal_sigdec (sd1, sd2)
        | (StrDec sd1, StrDec sd2) => equal_strdec (sd1, sd2)
        | (FunDec fd1, FunDec fd2) => equal_fundec (fd1, fd2)
        | (TopExp te1, TopExp te2) => equal_topexp (te1, te2)
        | _ => false


      fun equal_topelem (te1, te2) =
        let
          val checker =
            at #topdec equal_topdec <&> at #semicolon (equal_op equal_tok)
        in
          checker (te1, te2)
        end
    in
      Seq.equal equal_topelem (tops1, tops2)
    end

end
