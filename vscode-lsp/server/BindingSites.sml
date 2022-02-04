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
  val bindingSite: bs -> Token.t -> Token.t option

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


  fun bindingSite (BS {ids, binding, uses}) token =
    case TokenDict.find ids token of
      NONE => NONE
    | SOME i => IntDict.find binding i


  (** =======================================================================
    * =======================================================================
    * ======== everything below is the implementation of `fromAst`
    * =======================================================================
    * =======================================================================
    *)


  (** the loop context, used while we traverse the Ast to map identifiers to
    * their unique id
    *)
  structure Ctx:
  sig
    type t
    type ctx = t

    datatype kind = Val | Struct

    val empty: ctx
    val singleton: kind * string * int -> ctx
    val insert: ctx -> kind * string * int -> ctx
    val find: ctx -> kind * string -> int option
    val union: ctx * ctx -> ctx
  end =
  struct
    datatype kind = Val | Struct

    datatype t =
      Ctx of
        { vals: int StringDict.t
        , structs: int StringDict.t
        }

    type ctx = t

    val empty = Ctx
      { vals = StringDict.empty
      , structs = StringDict.empty
      }

    fun union (Ctx c1, Ctx c2) =
      Ctx
        { vals = StringDict.unionWith #2 (#vals c1, #vals c2)
        , structs = StringDict.unionWith #2 (#structs c1, #structs c2)
        }

    fun insert (Ctx {vals, structs}) (kind, var, i) =
      let
        val (vals, structs) =
          case kind of
            Val => (StringDict.insert vals (var, i), structs)
          | Struct => (vals, StringDict.insert structs (var, i))
      in
        Ctx {vals=vals, structs=structs}
      end

    fun singleton (kind, var, i) =
      insert empty (kind, var, i)

    fun find (Ctx {vals, structs}) (kind, var) =
      case kind of
        Val => StringDict.find vals var
      | Struct => StringDict.find structs var
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



  fun pat ((acc, ctx), p) =
    let
      open Ast.Pat
    in
      case p of
        Ident {id, ...} =>
          if MaybeLongToken.isLong id then
            (** Definitely a constructor, not a variable binding *)
            (acc, Ctx.empty)
          else
            (** TODO: need to know the constructors that are in scope to
              * determine whether or not this is a constructor binding.
              * For now, we'll assume it is.
              *)
            let
              val tok = MaybeLongToken.getToken id
              val (acc, i) = Acc.newBinding acc tok
            in
              (acc, Ctx.singleton (Ctx.Val, Token.toString tok, i))
            end

      | List {elems, ...} =>
          Seq.iterate (growCtx (withCtx ctx pat)) (acc, Ctx.empty) elems

      | Tuple {elems, ...} =>
          Seq.iterate (growCtx (withCtx ctx pat)) (acc, Ctx.empty) elems

      | Parens {pat=p, ...} =>
          pat ((acc, ctx), p)

      | Con {id, atpat, ...} =>
          (** the id here is definitely a constructor *)
          pat ((acc, ctx), atpat)

      | Infix {left, id, right} =>
          let
            val (acc, new1) = pat ((acc, ctx), left)
            val (acc, new2) = pat ((acc, ctx), right)
          in
            (acc, Ctx.union (new1, new2))
          end

      | Typed {pat=p, ty=t, ...} =>
          pat ((acc, ctx), p)

      | Layered {id, pat=p, ...} =>
          let
            val (acc, i) = Acc.newBinding acc id
            val new1 = Ctx.singleton (Ctx.Val, Token.toString id, i)
            val (acc, new2) = pat ((acc, ctx), p)
          in
            (acc, Ctx.union (new1, new2))
          end

      | Record {elems, ...} =>
          let
            fun patrow ((acc, ctx), pr) =
              case pr of
                LabEqPat {lab, pat=p, ...} =>
                  pat ((acc, ctx), p)

              | LabAsPat {id, ty=tty, aspat} =>
                  let
                    val (acc, i) = Acc.newBinding acc id
                    val new1 = Ctx.singleton (Ctx.Val, Token.toString id, i)

                    val (acc, new2) =
                      case aspat of
                        NONE => (acc, Ctx.empty)
                      | SOME {pat=p, ...} => pat ((acc, ctx), p)
                  in
                    (acc, Ctx.union (new1, new2))
                  end

              | _ => (acc, Ctx.empty)
          in
            Seq.iterate (growCtx (withCtx ctx patrow)) (acc, Ctx.empty) elems
          end

      | _ => (acc, Ctx.empty)
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
              case Ctx.find ctx (Ctx.Val, Token.toString tok) of
                NONE => acc
              | SOME i => Acc.newUse acc (tok, i)
            end

      | Tuple {elems, ...} =>
          Seq.iterate (exp ctx) acc elems

      | List {elems, ...} =>
          Seq.iterate (exp ctx) acc elems

      | Sequence {elems, ...} =>
          Seq.iterate (exp ctx) acc elems

      | LetInEnd {dec=d, exps, ...} =>
          let
            val (acc, ctx') = growCtx dec ((acc, ctx), d)
          in
            Seq.iterate (exp ctx') acc exps
          end

      | Parens {exp=e', ...} => exp ctx (acc, e')

      | App {left, right} => exp ctx (exp ctx (acc, left), right)

      | Infix {left, id, right} =>
          exp ctx (exp ctx (acc, left), right)

      | Typed {exp=e', ty=t, ...} =>
          exp ctx (acc, e')

      | Andalso {left, right, ...} =>
          exp ctx (exp ctx (acc, left), right)

      | Orelse {left, right, ...} =>
          exp ctx (exp ctx (acc, left), right)

      | IfThenElse {exp1, exp2, exp3, ...} =>
          exp ctx (exp ctx (exp ctx (acc, exp1), exp2), exp3)

      | Record {elems, ...} =>
          let
            fun patrow (acc, {lab, exp=e, ...}) =
              exp ctx (acc, e)
          in
            Seq.iterate patrow acc elems
          end

      | Case {exp=e', elems = clauses, ...} =>
          let
            fun clause (acc, {pat=p, exp=e', ...}) =
              let
                val (acc, ctx') = growCtx pat ((acc, ctx), p)
              in
                exp ctx' (acc, e')
              end
          in
            Seq.iterate clause (exp ctx (acc, e')) clauses
          end

      | Handle {exp=e', elems = clauses, ...} =>
          let
            fun clause (acc, {pat=p, exp=e', ...}) =
              let
                val (acc, ctx') = growCtx pat ((acc, ctx), p)
              in
                exp ctx' (acc, e')
              end
          in
            Seq.iterate clause (exp ctx (acc, e')) clauses
          end

      | Raise {exp=e', ...} => exp ctx (acc, e')

      | While {exp1, exp2, ...} =>
          exp ctx (exp ctx (acc, exp1), exp2)

      | Fn {elems = clauses, ...} =>
          let
            fun clause (acc, {pat=p, exp=e', ...}) =
              let
                val (acc, ctx') = growCtx pat ((acc, ctx), p)
              in
                exp ctx' (acc, e')
              end
          in
            Seq.iterate clause acc clauses
          end

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
                (acc, Ctx.insert ctx (Ctx.Val, Token.toString tok, i))
              end

            val (acc, newNames) =
              Seq.iterate bindname (acc, Ctx.empty) mutuals

            val ctx' = Ctx.union (ctx, newNames)

            fun args ((acc, ctx), fna) =
              case fna of
                PrefixedFun {id, args, ...} =>
                  Seq.iterate (growCtx (withCtx ctx pat)) (acc, Ctx.empty) args
              | InfixedFun {id, larg, rarg} =>
                  let
                    val (acc, new1) = pat ((acc, ctx), larg)
                    val (acc, new2) = pat ((acc, ctx), rarg)
                  in
                    (acc, Ctx.union (new1, new2))
                  end
              | CurriedInfixedFun {id, larg, rarg, args, ...} =>
                  let
                    val (acc, new1) = pat ((acc, ctx), larg)
                    val (acc, new2) = pat ((acc, ctx), rarg)
                    val (acc, new3) =
                      Seq.iterate (growCtx (withCtx ctx pat)) (acc, Ctx.empty) args
                  in
                    (acc, Ctx.union (Ctx.union (new1, new2), new3))
                  end

            fun clause (acc, {fname_args=fna, exp=e, ...}) =
              let
                val (acc, new) = args ((acc, ctx'), fna)
              in
                exp (Ctx.union (ctx', new)) (acc, e)
              end

            fun mutualfunc (acc, {elems = clauses, ...}) =
              Seq.iterate clause acc clauses
          in
            (Seq.iterate mutualfunc acc mutuals, newNames)
          end

      | DecVal {elems = mutuals, ...} =>
          let
            fun mutualval ((acc, ctx), {pat=p, exp=e, ...}) =
              let
                val acc = exp ctx (acc, e)
                val (acc, new) = pat ((acc, ctx), p)
              in
                (acc, new)
              end
          in
            Seq.iterate (growCtx (withCtx ctx mutualval)) (acc, Ctx.empty) mutuals
          end

      | DecLocal {left_dec, right_dec, ...} =>
          let
            val (acc, ctx') = growCtx dec ((acc, ctx), left_dec)
          in
            dec ((acc, ctx'), right_dec)
          end
(*

      | DecInfix {elems, ...} =>
      | DecInfixr {elems, ...} =>
      | DecNonfix {elems, ...} =>
      | DecDatatype {datbind=db, withtypee = SOME {typbind = tb, ...}, ...} =>
      | DecDatatype {datbind=db, withtypee = NONE, ...} =>
      | DecOpen {elems, ...} =>
*)

      | _ => (acc, Ctx.empty)
    end



  fun strexp ctx (acc, e) =
    case e of
      Ast.Str.Ident id =>
        if MaybeLongToken.isLong id then
          (** TODO: qualified bindings... *)
          acc
        else
          let
            val tok = MaybeLongToken.getToken id
          in
            case Ctx.find ctx (Ctx.Struct, Token.toString tok) of
              NONE => acc
            | SOME i => Acc.newUse acc (tok, i)
          end
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
          fun mutualstruct ((acc, ctx), {strid, strexp=se, constraint, ...}) =
            let
              val (acc, i) = Acc.newBinding acc strid
              val new = Ctx.singleton (Ctx.Struct, Token.toString strid, i)
            in
              (strexp ctx (acc, se), new)
            end
        in
          Seq.iterate (growCtx (withCtx ctx mutualstruct)) (acc, Ctx.empty) mutuals
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
