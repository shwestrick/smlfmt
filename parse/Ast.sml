(** Copyright (c) 2020-2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)


(** See AstType.sml for the type definition. *)
structure Ast =
struct

  open AstType

  structure MaybeLong =
  struct
    open AstType.MaybeLong
  end

  structure SyntaxSeq =
  struct
    open AstType.SyntaxSeq
  end

  structure Ty =
  struct
    open AstType.Ty
  end

  structure Pat =
  struct
    open AstType.Pat

    fun okayForConPat pat =
      case pat of
        Ident _ => true
      | _ => false

    fun unpackForConPat pat =
      case pat of
        Ident {opp, id} => {opp=opp, id=id}
      | _ => raise Fail "Bug: Ast.Pat.unpackForConPat: invalid argument"

    fun isValueIdentifier pat =
      case pat of
        Ident {id, ...} =>
          Token.isValueIdentifierNoEqual (MaybeLong.getToken id)
      | _ => false

    fun okayForAsPat pat =
      isValueIdentifier pat
      orelse
      case pat of
        Typed {pat, ...} =>
          isValueIdentifier pat
      | _ => false

    (** Not that the unpacked id has to be a "short" identifer. We could
      * check this here for sanity...
      *)
    fun unpackForAsPat pat =
      case pat of
        Ident {opp, id} =>
          { opp = opp
          , id = MaybeLong.getToken id
          , ty = NONE
          }
      | Typed {pat = Ident {opp, id}, colon, ty} =>
          { opp = opp
          , id = MaybeLong.getToken id
          , ty = SOME {colon = colon, ty = ty}
          }
      | _ => raise Fail "Bug: Ast.Pat.unpackForAsPat: invalid argument"

    fun isAtPat pat =
      case pat of
        Wild _ => true
      | Const _ => true
      | Unit _ => true
      | Ident _ => true
      | List _ => true
      | Tuple _ => true
      | Record _ => true
      | Parens _ => true
      | _ => false

    fun isAppPat pat =
      isAtPat pat orelse
      (case pat of
        Con _ => true
      | _ => false)

    fun isInfPat pat =
      isAppPat pat orelse
      (case pat of
        Infix _ => true
      | _ => false)
  end

  structure Exp =
  struct
    open AstType.Exp

    fun isAtExp exp =
      case exp of
        Const _ => true
      | Ident _ => true
      | Record _ => true
      | Select _ => true
      | Unit _ => true
      | Tuple _ => true
      | List _ => true
      | Sequence _ => true
      | LetInEnd _ => true
      | Parens _ => true
      | _ => false

    fun isAppExp exp =
      isAtExp exp orelse
      (case exp of
        App _ => true
      | _ => false)

    fun isInfExp exp =
      isAppExp exp orelse
      (case exp of
        Infix _ => true
      | _ => false)

    fun isMultipleDecs dec =
      case dec of
        DecMultiple {elems, ...} =>
          Seq.length elems > 1
      | _ => false
  end


  structure Str =
  struct
    open AstType.Str

    fun isMultipleDecs dec =
      case dec of
        DecMultiple {elems, ...} =>
          Seq.length elems > 1
      | _ => false
  end

end
