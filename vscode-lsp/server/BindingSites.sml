structure BindingSites:
sig
  type t
  type bs = t

  (** If the input token is an identifier (variable, constructor, etc.),
    * return where that identifier was bound (e.g. as an argument of an
    * enclosing function, inside a pattern of an early let-binding, etc.)
    *
    * If the input is not an identifier or is free (not bound) within the
    * scope of the given AST, returns NONE.
    *)
  (* val bindingSite: bs -> Token.t -> Token.t option *)

  val fromAst: Ast.t -> bs
  val toString: bs -> string
end =
struct

  structure TokenKey =
  struct
    type t = Token.t

    (** Simple way to compare tokens: within a file, each token has as unique
      * start offset. Otherwise, we can just compare the name of the files they
      * came from.
      *)
    fun compare (tok1, tok2) =
      let
        val src1 = Token.getSource tok1
        val src2 = Token.getSource tok2
        fun soff src = Source.absoluteStartOffset src
        fun pathstr s = FilePath.toUnixPath (Source.fileName s)
      in
        case Int.compare (soff src1, soff src2) of
          LESS => LESS
        | GREATER => GREATER
        | EQUAL => String.compare (pathstr src1, pathstr src2)
      end
  end

  structure StringKey =
    struct type t = string val compare = String.compare end

  structure IntKey =
    struct type t = int val compare = Int.compare end

  structure TokenDict = Dict(TokenKey)
  structure StringDict = Dict(StringKey)
  structure IntDict = Dict(IntKey)

  (** high level idea is to assign a unique identifier (integer) to each
    * identifier token, and then for each unique identifier, we keep track
    * of both (a) where it was bound, and (b) all places it was used.
    *)
  datatype t =
    BS of
      { ids: int TokenDict.t
      , binding: Token.t IntDict.t
      , uses: (Token.t list) IntDict.t
      }
  type bs = t

  fun toString (BS {ids, binding, uses}) =
    let
      fun oneUse tok =
        let
          val {line, col} = Source.absoluteStart (Token.getSource tok)
        in
          "  line " ^ Int.toString line ^ ", col " ^ Int.toString col
        end

      fun oneBinding (i, tok) =
        let
          val {line, col} = Source.absoluteStart (Token.getSource tok)
          val uses = IntDict.lookup uses i
        in
          "line " ^ Int.toString line ^ ", col " ^ Int.toString col ^ ": "
          ^ Token.toString tok ^ " (" ^ Int.toString i ^ ")"
          ^ (case uses of [] => "" | _ => ": uses:\n"
               ^ String.concatWith "\n" (List.map oneUse uses))
        end
    in
      String.concatWith "\n" (List.map oneBinding (IntDict.toList binding))
      ^ "\n"
    end


  (** the loop context, used while we traverse the Ast to map identifiers to
    * their unique id
    *)
  structure Ctx:
  sig
    type t
    type ctx = t

    val empty: ctx
    val singleton: string * int -> ctx
    val insert: ctx -> string * int -> ctx
    val find: ctx -> string -> int option
    val union: ctx * ctx -> ctx
  end =
  struct
    datatype t =
      Ctx of
        { vars: int StringDict.t } (* TODO: structure ids... *)

    type ctx = t

    val empty = Ctx {vars = StringDict.empty}

    fun singleton (var, i) =
      Ctx {vars = StringDict.singleton (var, i)}

    fun union (Ctx {vars=v1}, Ctx {vars=v2}) =
      Ctx {vars = StringDict.unionWith #2 (v1, v2)}

    fun insert (Ctx {vars}) (var, i) =
      Ctx {vars = StringDict.insert vars (var, i)}

    fun find (Ctx {vars}) var =
      StringDict.find vars var
  end


  (** the loop accumulator, used while we traverse the Ast *)
  structure Acc:
  sig
    type t
    type acc = t
    val initial: acc
    (* val newUnique: acc -> (acc * int) *)
    val newBinding: acc -> Token.t -> acc * int
    val newUse: acc -> Token.t * int -> acc
    val bs: acc -> bs
  end =
  struct
    datatype t =
      Acc of
        { bs: bs
        , nextUnique: int
        }

    type acc = t

    val initial =
      Acc
        { nextUnique = 0
        , bs = BS
            { ids = TokenDict.empty
            , binding = IntDict.empty
            , uses = IntDict.empty
            }
        }

    (* fun newUnique (Acc {bs, nextUnique}) =
      (Acc {bs=bs, nextUnique=nextUnique+1}, nextUnique) *)

    fun newBinding (Acc {nextUnique, bs = BS {ids, binding, uses}}) tok =
      let
        val i = nextUnique
        val ids = TokenDict.insert ids (tok, i)
        val binding = IntDict.insert binding (i, tok)
        val uses = IntDict.insert uses (i, [])

        val result = Acc
          { nextUnique = i+1
          , bs = BS
              { ids = ids
              , binding = binding
              , uses = uses
              }
          }
      in
        (result, i)
      end

    fun newUse (Acc {nextUnique, bs = BS {ids, binding, uses}}) (tok, i) =
      let
        val ids = TokenDict.insert ids (tok, i)
        val otherUses = IntDict.lookup uses i
        val uses = IntDict.insert uses (i, tok :: otherUses)
      in
        Acc
          { nextUnique = nextUnique
          , bs = BS
              { ids = ids
              , binding = binding
              , uses = uses
              }
          }
      end

    fun bs (Acc {bs=x, ...}) = x
  end


  (** =======================================================================
    * looping functions below, for traversing the ast.
    *
    * two kinds of functions, based on their types:
    *
    *   ctx -> acc * X -> acc
    *     operates on things of type X, doesn't extend the context
    *
    *   (acc * ctx) * X -> acc * ctx
    *     operates on things of type X, extends the context. In this case,
    *     the new stuff in the context is returned separately (i.e., the
    *     extended context is the union of the input and output)
    *)



  (** This can be used to carry the context through, as though it were part
    * of the accumulator.
    *
    * For example:
    *   iterate (growCtx dec) (acc, ctx) stuff
    * vs
    *   iterate dec (acc, ctx) stff
    *
    * In the latter, each successive dec only has access to the bindings
    * made by the previous dec. In the former, it has access to _all_ previous.
    *)
  fun growCtx (f: (Acc.t * Ctx.t) * 'a -> Acc.t * Ctx.t) ((acc, ctx), x) =
    let
      val (acc, new) = f ((acc, ctx), x)
    in
      (acc, Ctx.union (ctx, new))
    end


  fun withCtx ctxOuter (f: (Acc.t * Ctx.t) * 'a -> Acc.t * Ctx.t) ((acc, ctx), x) =
    let
      val ctx' = Ctx.union (ctxOuter, ctx)
    in
      f ((acc, ctx'), x)
    end



  fun exp ctx (acc, e) =
    let
      open Ast.Exp
    in
      case e of
        Ident {id, ...} =>
          if MaybeLongToken.isLong id then
            acc
          else
            let
              val tok = MaybeLongToken.getToken id
            in
              case Ctx.find ctx (Token.toString tok) of
                NONE => acc
              | SOME i => Acc.newUse acc (tok, i)
            end

        (* Tuple {elems, ...} =>
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
          add (id, InfixOp) (exp (exp (acc, left), right))

      | Typed {exp=e', ty=t, ...} => exp (ty (acc, t), e')

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
              add (lab, Label) (exp (acc, e))
          in
            Seq.iterate patrow acc elems
          end *)

      | _ => acc
    end


  and dec ((acc, ctx), d) =
    let
      open Ast.Exp
    in
      case d of
        DecMultiple {elems, ...} =>
          Seq.iterate (growCtx (withCtx ctx dec)) (acc, Ctx.empty) elems

      | DecFun {fvalbind = {elems = mutuals, ...}, ...} =>
          let
            fun name fna =
              case fna of
                PrefixedFun {id, args, ...} => id
              | InfixedFun {id, larg, rarg} => id
              | CurriedInfixedFun {id, larg, rarg, args, ...} => id

            fun bindname ((acc, ctx), {elems=clauses, ...}) =
              let
                val {fname_args, ...} = Seq.nth clauses 0
                val tok = name fname_args
                val (acc, i) = Acc.newBinding acc tok
              in
                (acc, Ctx.insert ctx (Token.toString tok, i))
              end

            val (acc, newNames) =
              Seq.iterate bindname (acc, Ctx.empty) mutuals

            val ctx' = Ctx.union (ctx, newNames)

            fun clause (acc, {exp=e, ...}) =
              exp ctx' (acc, e)

            fun mutualfunc (acc, {elems = clauses, ...}) =
              Seq.iterate clause acc clauses
          in
            (Seq.iterate mutualfunc acc mutuals, newNames)
          end

      (*| DecVal {elems = mutuals, ...} =>
          let
            fun mutualval (acc, {pat=p, exp=e, ...}) =
              pat (exp (acc, e), p)
          in
            Seq.iterate mutualval acc mutuals
          end

      | DecLocal {left_dec, right_dec, ...} =>
          dec (dec (acc, left_dec), right_dec)


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
              add (MaybeLongToken.getToken t, StructureId) acc
          in
            Seq.iterate elem acc elems
          end *)

      | _ => (acc, Ctx.empty)
    end



  fun strexp ctx (acc, e) =
    case e of
      Ast.Str.Ident x =>
        (* TODO: structure/functor bindings *)
        acc
    | Ast.Str.Struct {structt, strdec=sd, endd} =>
        let
          val (acc, _) = strdec ((acc, ctx), sd)
        in
          acc
        end
    | Ast.Str.Constraint {strexp=se, ...} =>
        strexp ctx (acc, se)
    | Ast.Str.FunAppExp {funid, strexp=se, ...} =>
        (* TODO: structure/functor bindings *)
        strexp ctx (acc, se)
    | Ast.Str.FunAppDec {funid, strdec=sd, ...} =>
        (* TODO: structure/functor bindings *)
        let
          val (acc, _) = strdec ((acc, ctx), sd)
        in
          acc
        end
    | Ast.Str.LetInEnd {strdec=sd, strexp=se, ...} =>
        let
          val (acc, ctx') = growCtx strdec ((acc, ctx), sd)
        in
          strexp ctx' (acc, se)
        end


  and strdec ((acc, ctx), d) =
    case d of
      Ast.Str.DecMultiple {elems, ...} =>
        Seq.iterate (growCtx (withCtx ctx strdec)) (acc, Ctx.empty) elems
    | Ast.Str.DecCore cd =>
        dec ((acc, ctx), cd)
    | Ast.Str.DecStructure {elems = mutuals, ...} =>
        let
          fun mutualstruct (acc, {strid, strexp=se, constraint, ...}) =
            (* TODO: structure/functor bindings *)
            strexp ctx (acc, se)
        in
          (Seq.iterate mutualstruct acc mutuals, Ctx.empty)
        end
    | Ast.Str.DecLocalInEnd {strdec1, strdec2, ...} =>
        let
          val (acc, ctx') = growCtx strdec ((acc, ctx), strdec1)
          val (acc, new) = strdec ((acc, ctx'), strdec2)
        in
          (acc, new)
        end
    | _ => (acc, Ctx.empty)


  fun fundec ((acc, ctx), d) =
    case d of
      Ast.Fun.DecFunctor {elems = mutuals, ...} =>
        let
          fun mutualfun (acc, {funid, strexp=se, ...}) =
            (* TODO: structure/functor bindings *)
            strexp ctx (acc, se)
        in
          (Seq.iterate mutualfun acc mutuals, Ctx.empty)
        end

  fun fromAst (Ast.Ast topdecs) =
    let
      fun topdec ((acc, ctx), {topdec=d, ...}) =
        case d of
          Ast.StrDec sd => strdec ((acc, ctx), sd)
        | Ast.FunDec fd => fundec ((acc, ctx), fd)
        | _ => (acc, Ctx.empty)

      val (final, _) =
        Seq.iterate (growCtx topdec) (Acc.initial, Ctx.empty) topdecs
    in
      Acc.bs final
    end

end
