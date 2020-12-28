(** Copyright (c) 2020 Sam Westrick
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

    fun make (tok: Token.t) : t =
      case Token.getClass tok of
        Token.Identifier => tok
      | Token.LongIdentifier => tok
      | cls =>
          raise Fail ("Ast.MaybeLong.make: given non-identifier ("
                      ^ Token.classToString cls ^ ")")
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
  end

  structure Exp =
  struct
    open AstType.Exp
  end

end
