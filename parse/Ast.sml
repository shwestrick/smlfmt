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
